import subprocess
import tempfile
import os
import google.generativeai as genai
from dotenv import load_dotenv

class JavaValidator:
    def __init__(self):
        self.temp_dir = tempfile.mkdtemp()
        load_dotenv()
        # Configure with the correct API version
        genai.configure(
            api_key=os.getenv('GEMINI_API_KEY'),
            transport='rest'  # Use REST transport instead of gRPC
        )
        # Use the free tier model
        self.model = genai.GenerativeModel('gemini-1.5-flash')
    
    def compile_java(self, java_code: str) -> tuple[bool, str]:
        """Compile Java code and return success status and error message."""
        # Create temporary Java file
        temp_file = os.path.join(self.temp_dir, "Generated.java")
        with open(temp_file, 'w') as f:
            f.write(java_code)
        
        # Try to compile
        try:
            result = subprocess.run(
                ['javac', temp_file],
                capture_output=True,
                text=True
            )
            return result.returncode == 0, result.stderr
        except Exception as e:
            return False, str(e)
    
    def check_logical_equivalence(self, code1: str, code2: str) -> tuple[bool, str]:
        """Check if two Java code snippets are logically equivalent."""
        prompt = f"""
        Compare these two Java code snippets and determine if they are logically equivalent.
        They may have different variable names, different formatting, or different implementation approaches,
        but they should perform the same operations and produce the same results.
        
        Code 1:
        {code1}
        
        Code 2:
        {code2}
        
        Please analyze:
        1. Input/output behavior
        2. Control flow
        3. Data transformations
        4. Edge cases handling
        
        Return your analysis in this format:
        EQUIVALENT: [true/false]
        REASON: [brief explanation]
        """
        
        try:
            response = self.model.generate_content(prompt)
            analysis = response.text
            
            # Parse the response
            is_equivalent = "EQUIVALENT: true" in analysis
            reason = analysis.split("REASON:")[1].strip() if "REASON:" in analysis else "No reason provided"
            
            return is_equivalent, reason
        except Exception as e:
            print(f"Error checking logical equivalence: {str(e)}")
            return False, "Error during analysis"
    
    def compare_code(self, code1: str, code2: str) -> str:
        """Compare two Java code snippets and return detailed analysis."""
        # First check logical equivalence
        is_equivalent, reason = self.check_logical_equivalence(code1, code2)
        
        if is_equivalent:
            return "The code is logically equivalent. " + reason
        
        # If not logically equivalent, do a structural comparison
        lines1 = code1.split('\n')
        lines2 = code2.split('\n')
        
        differences = []
        for i, (line1, line2) in enumerate(zip(lines1, lines2)):
            if line1.strip() != line2.strip():
                differences.append(f"Line {i+1}:")
                differences.append(f"Generated: {line1}")
                differences.append(f"Expected:  {line2}")
        
        # Add the logical equivalence analysis
        differences.append("\nLogical Analysis:")
        differences.append(reason)
        
        return '\n'.join(differences) if differences else "No structural differences found"
    
    def cleanup(self):
        """Clean up temporary files."""
        for file in os.listdir(self.temp_dir):
            os.remove(os.path.join(self.temp_dir, file))
        os.rmdir(self.temp_dir) 