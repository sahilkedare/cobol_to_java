from typing import TypedDict
from langgraph.graph import Graph, END
from cobol_processor import COBOLProcessor
from java_validator import JavaValidator
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class State(TypedDict):
    cobol_code: str
    java_code: str
    expected_java: str | None
    error: str
    iteration: int

def create_workflow() -> Graph:
    """Create the modernization workflow."""
    # Initialize components
    cobol_processor = COBOLProcessor()
    java_validator = JavaValidator()
    
    def generate_java(state: State) -> State:
        """Generate Java code from COBOL."""
        logger.info(f"Starting Java code generation (Iteration {state['iteration'] + 1})")
        try:
            state["java_code"] = cobol_processor.generate_java(state["cobol_code"])
            state["iteration"] += 1
            logger.info("Java code generated successfully")
        except Exception as e:
            state["error"] = f"Error generating Java code: {str(e)}"
            logger.error(f"Failed to generate Java code: {str(e)}")
        return state
    
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
    
    def fix_java(state: State) -> State:
        """Fix Java code if needed."""
        if state["error"]:
            logger.info("Attempting to fix Java code...")
            try:
                state["java_code"] = cobol_processor.fix_java(state["java_code"], state["error"])
                logger.info("Java code fixed successfully")
            except Exception as e:
                state["error"] = f"Error fixing Java code: {str(e)}"
                logger.error(f"Failed to fix Java code: {str(e)}")
        return state
    
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
    
    def should_continue(state: State) -> bool:
        """Check if we should continue iterating."""
        if state["iteration"] >= 7:  # Max 7 iterations
            logger.info("Maximum iterations reached")
            return False
        if not state["error"]:  # No errors or differences
            logger.info("No errors or differences found, stopping iteration")
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
    
    # Add edges
    workflow.add_edge("generate", "validate")
    workflow.add_edge("validate", "fix")
    workflow.add_edge("fix", "compare")
    
    # Set entry point
    workflow.set_entry_point("generate")
    
    # Add conditional edges
    workflow.add_conditional_edges(
        "compare",
        should_continue,
        {
            True: "generate",
            False: END
        }
    )
    
    return workflow.compile()

def run_workflow(cobol_file: str, expected_java_file: str | None = None) -> State:
    """Run the modernization workflow."""
    logger.info(f"Starting workflow for COBOL file: {cobol_file}")
    if expected_java_file:
        logger.info(f"Expected Java file provided: {expected_java_file}")
    
    # Initialize state
    cobol_processor = COBOLProcessor()
    state: State = {
        "cobol_code": cobol_processor.read_file(cobol_file),
        "java_code": "",
        "expected_java": cobol_processor.read_file(expected_java_file) if expected_java_file else None,
        "error": "",
        "iteration": 0
    }
    
    # Run workflow
    workflow = create_workflow()
    try:
        logger.info("Executing workflow...")
        result = workflow.invoke(state)
        logger.info("Workflow completed successfully")
        return result
    except Exception as e:
        logger.error(f"Workflow failed: {str(e)}")
        return state 