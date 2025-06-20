import argparse
import os
import logging
from dotenv import load_dotenv
from workflow import run_workflow
from java_validator import JavaValidator
from cobol_processor import COBOLProcessor
from easytrieve_processor import EASYTRIEVEProcessor

# Load environment variables
load_dotenv()

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def is_valid_input_file(file_path: str) -> bool:
    """Check if the file has a valid extension."""
    valid_extensions = {'.cbl', '.cob', '.cobol', '.cpy', '.txt', '.ezr', '.ez'}
    return os.path.splitext(file_path)[1].lower() in valid_extensions

def get_processor(file_path: str):
    """Get the appropriate processor based on file extension."""
    ext = os.path.splitext(file_path)[1].lower()
    if ext in {'.cbl', '.cob', '.cobol', '.cpy'}:
        return COBOLProcessor()
    elif ext in {'.ezr', '.ez', '.txt'}:
        # For .txt files, we need to check the content to determine if it's EASYTRIEVE
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read().lower()
            if any(keyword in content for keyword in ['file', 'define', 'sort', 'report']):
                return EASYTRIEVEProcessor()
        return COBOLProcessor()  # Default to COBOL if no EASYTRIEVE keywords found
    return None

def save_java_code(java_code: str, input_file: str) -> str:
    """Save Java code to a file and return the file path."""
    # Create output directory if it doesn't exist
    output_dir = "generated_java"
    os.makedirs(output_dir, exist_ok=True)
    
    # Generate Java file name from input file name
    base_name = os.path.splitext(os.path.basename(input_file))[0]
    java_file = os.path.join(output_dir, f"{base_name}.java")
    
    # Save the Java code, overwriting if file exists
    try:
        with open(java_file, 'w', encoding='utf-8') as f:
            f.write(java_code)
        logger.info(f"Java code saved to: {java_file}")
    except Exception as e:
        logger.error(f"Error saving Java code: {str(e)}")
        raise
    
    return java_file

def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='COBOL/EASYTRIEVE to Java Modernization Tool')
    parser.add_argument('input_file', help='Path to the input file (.cbl, .cob, .cobol, .cpy, .txt, .ezr, .ez)')
    parser.add_argument('--expected-java', help='Path to the expected Java file (optional)')
    args = parser.parse_args()
    
    logger.info(f"Starting conversion for file: {args.input_file}")
    
    # Validate file extension
    if not is_valid_input_file(args.input_file):
        logger.error("Invalid file extension. Supported extensions are: .cbl, .cob, .cobol, .cpy, .txt, .ezr, .ez")
        return
    
    # Check if file exists
    if not os.path.exists(args.input_file):
        logger.error(f"File not found: {args.input_file}")
        return
    
    # Get the appropriate processor
    processor = get_processor(args.input_file)
    if not processor:
        logger.error("Could not determine the appropriate processor for the input file")
        return
    
    try:
        # Run the workflow
        logger.info("Initiating modernization workflow...")
        result = run_workflow(args.input_file, args.expected_java, processor)
        
        # Print results and save Java code
        if result and "java_code" in result and result["java_code"]:
            logger.info("Workflow completed. Generated Java code:")
            print("\nGenerated Java Code:")
            print(result["java_code"])
            
            # Save the Java code to a file
            java_file = save_java_code(result["java_code"], args.input_file)
            
            if result.get("error"):
                logger.warning(f"Workflow completed with warnings: {result['error']}")
                print("\nStatus:", result["error"])
            else:
                logger.info("Workflow completed successfully")
                print("\nStatus: Successfully generated Java code")
        else:
            logger.error("Failed to generate Java code")
            print("\nError: Failed to generate Java code")
            
    except Exception as e:
        logger.error(f"An error occurred: {str(e)}")
        print(f"\nError: {str(e)}")
    
    # Cleanup
    logger.info("Cleaning up resources...")
    validator = JavaValidator()
    validator.cleanup()
    logger.info("Cleanup completed")

if __name__ == "__main__":
    main() 