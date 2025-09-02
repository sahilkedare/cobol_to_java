from typing import TypedDict
from langgraph.graph import Graph, END
from cobol_processor import COBOLProcessor
from java_validator import JavaValidator
import logging
from langsmith import Client
from langsmith.run_helpers import traceable
import os
import re

# Set environment variables for LangSmith
os.environ["LANGSMITH_TRACING"] = "true"
os.environ["LANGSMITH_API_KEY"] = "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"
# os.environ["LANGSMITH_PROJECT"] = "java_cobol_agent"
os.environ["LANGSMITH_ENDPOINT"] = "https://api.smith.langchain.com"

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# Initialize LangSmith client with project
client = Client(
    api_key=os.environ["LANGSMITH_API_KEY"],
    api_url=os.environ["LANGSMITH_ENDPOINT"],
    # project_name=os.environ["LANGSMITH_PROJECT"]
)

class State(TypedDict):
    cobol_code: str
    java_code: str
    expected_java: str | None
    error: str
    iteration: int
    logical_differences: list[str]  

def create_workflow(processor) -> Graph:
    """Create the modernization workflow."""
    # Initialize components
    java_validator = JavaValidator()
    
    @traceable(run_type="tool", name="Generate Java Code")
    def generate_java(state: State) -> State:
        """Generate Java code from input code."""
        logger.info(f"Starting Java code generation (Iteration {state['iteration'] + 1})")
        try:
            state["java_code"] = processor.generate_java(state["cobol_code"])
            state["iteration"] += 1
            logger.info("Java code generated successfully")
        except Exception as e:
            state["error"] = f"Error generating Java code: {str(e)}"
            logger.error(f"Failed to generate Java code: {str(e)}")
        return state
    
    @traceable(run_type="tool", name="Validate Java Code")
    def validate_java(state: State) -> State:
        """Validate Java code."""
        logger.info("Validating Java code...")
        try:
            success, error = java_validator.compile_java(state["java_code"])
            state["error"] = error
            if success:
                logger.info("Java code compiled successfully")
            else:
                logger.warning(f"Java compilation failed: {error}")
        except Exception as e:
            state["error"] = f"Error validating Java code: {str(e)}"
            logger.error(f"Failed to validate Java code: {str(e)}")
        return state
    
    @traceable(run_type="tool", name="Fix Java Code")
    def fix_java(state: State) -> State:
        """Fix Java code considering both syntax and logical errors."""
        if state["error"] or state.get("logical_differences"):
            logger.info("Attempting to fix Java code...")
            try:
                # Combine both syntax and logical issues for fixing
                issues_to_fix = []
                if state["error"]:
                    issues_to_fix.append(f"Syntax error: {state['error']}")
                if state.get("logical_differences"):
                    issues_to_fix.append(f"Logical differences: {state['logical_differences'][0]}")
                
                combined_issues = "\n".join(issues_to_fix)
                
                # Enhanced fix prompt that considers both syntax and logic
                fix_prompt = f"""Fix the Java code to address both syntax and logical issues:
                
                Original COBOL code for reference:
                {state["cobol_code"]}
                
                Current Java code:
                {state["java_code"]}
                
                Issues to fix:
                {combined_issues}
                
                Please provide corrected Java code that maintains logical equivalence with the COBOL code.
                """
                
                state["java_code"] = processor.fix_java(state["java_code"], fix_prompt)
                logger.info("Java code fixed successfully")
                
            except Exception as e:
                state["error"] = f"Error fixing Java code: {str(e)}"
                logger.error(f"Failed to fix Java code: {str(e)}")
        return state
    
    @traceable(run_type="tool", name="Compare Java Code")
    def compare_java(state: State) -> State:
        """Compare with expected Java if provided."""
        if state["expected_java"]:
            logger.info("Comparing with expected Java code...")
            try:
                comparison_result = java_validator.compare_code(state["java_code"], state["expected_java"])
                # Only set error if the comparison shows actual differences
                if not comparison_result.startswith("The code is logically equivalent"):
                    state["error"] = comparison_result
                    logger.warning(f"Code differences found: {comparison_result}")
                else:
                    state["error"] = ""  # Clear error if logically equivalent
                    logger.info("Code is logically equivalent to expected output")
                    print("\n✅ Generated Java code is logically equivalent to expected code!")
            except Exception as e:
                state["error"] = f"Error comparing Java code: {str(e)}"
                logger.error(f"Failed to compare Java code: {str(e)}")
        return state

    @traceable(run_type="tool", name="Compare COBOL-Java Logic")
    def compare_cobol_java_logic(state: State) -> State:
        """Compare core logical equivalence using LLM."""
        logger.info("Analyzing COBOL-Java logical equivalence...")
        try:
            prompt = f"""Compare only the core business logic equivalence of these programs:
            
            COBOL Code:
            {state["cobol_code"]}
            
            Java Code:
            {state["java_code"]}
            
            Focus ONLY on:
            1. Core functionality (what the program actually does)
            2. Basic data flow
            3. Essential business rules
            
            Ignore differences in:
            - Language-specific I/O operations
            - Data type precision
            - Programming paradigms
            
            Are they functionally equivalent? Answer with EQUIVALENT or NOT EQUIVALENT, 
            followed by ONLY critical logical differences (if any).
            """
                
            # Use the model to compare
            response = processor.model.generate_content(prompt).text
            
            if "NOT EQUIVALENT" in response.upper():
                # Extract only the critical differences
                differences = response.split("NOT EQUIVALENT", 1)[1].strip()
                state["logical_differences"] = [differences]
                state["error"] = f"Core logical differences: {differences}"
                logger.warning(f"Core logical differences found: {differences}")
            else:
                state["logical_differences"] = []
                state["error"] = ""
                logger.info("Core business logic is equivalent")
                print("\n✅ COBOL and Java code are functionally equivalent!")

        except Exception as e:
            state["error"] = f"Error in logical comparison: {str(e)}"
            logger.error(f"Comparison failed: {str(e)}")

        return state    
    
    def should_continue(state: State) -> bool:
        """Check if we should continue iterating."""
        if state["iteration"] >= 7:  # Max 7 iterations
            logger.info("Maximum iterations reached")
            return False
        
        has_no_errors = not state["error"]
        is_logically_equivalent = not state.get("logical_differences", [])
    
        if has_no_errors and is_logically_equivalent:
            logger.info("No syntactic or logical differences found, stopping iteration")
            return False

        if state["expected_java"] and state["error"].startswith("The code is logically equivalent"):
            logger.info("Code is logically equivalent to expected output, stopping iteration")
            return False
        logger.info("Continuing iteration due to errors or differences")
        return True
    
    # Create the workflow
    workflow = Graph()
    
    # Add nodes
    workflow.add_node("generate", generate_java)
    workflow.add_node("validate", validate_java)
    workflow.add_node("fix", fix_java)
    workflow.add_node("compare", compare_java)
    workflow.add_node("compare_logic", compare_cobol_java_logic)
    
    # Add edges
    workflow.add_edge("generate", "validate")
    workflow.add_edge("validate", "fix")
    workflow.add_edge("fix", "compare")
    workflow.add_edge("compare", "compare_logic")
    
    # Set entry point
    workflow.set_entry_point("generate")
    
    # Add conditional edges
    workflow.add_conditional_edges(
        "compare_logic",
        should_continue,
        {
            True: "generate",
            False: END
        }
    )
    
    return workflow.compile()

@traceable(run_type="chain", name="COBOL to Java Conversion Workflow")
def run_workflow(input_file: str, expected_java_file: str | None = None, processor = None) -> State:
    """Run the modernization workflow."""
    logger.info(f"Starting workflow for file: {input_file}")
    if expected_java_file:
        logger.info(f"Expected Java file provided: {expected_java_file}")
    
    # Initialize state
    state: State = {
        "cobol_code": processor.read_file(input_file),
        "java_code": "",
        "expected_java": processor.read_file(expected_java_file) if expected_java_file else None,
        "error": "",
        "iteration": 0,
        "logical_differences": []
    }
    
    # Run workflow
    workflow = create_workflow(processor)
    try:
        logger.info("Executing workflow...")
        result = workflow.invoke(state)
        logger.info("Workflow completed successfully")
        return result
    except Exception as e:
        logger.error(f"Workflow failed: {str(e)}")
        return state