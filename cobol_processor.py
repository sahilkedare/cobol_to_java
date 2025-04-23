import google.generativeai as genai
import os
from dotenv import load_dotenv

class COBOLProcessor:
    def __init__(self):
        load_dotenv()
        # Configure with the correct API version
        genai.configure(
            api_key=os.getenv('GEMINI_API_KEY'),
            transport='rest'  # Use REST transport instead of gRPC
        )
        # Use the free tier model
        self.model = genai.GenerativeModel('gemini-1.5-flash')
    
    def read_file(self, file_path: str) -> str:
        """Read a COBOL file and return its contents."""
        try:
            with open(file_path, 'r', encoding='utf-8') as file:
                content = file.read()
                
            # Check if file is empty
            if not content.strip():
                raise ValueError(f"File is empty: {file_path}")
                
            return content
        except UnicodeDecodeError:
            # Try with different encoding if UTF-8 fails
            try:
                with open(file_path, 'r', encoding='latin-1') as file:
                    return file.read()
            except Exception as e:
                raise ValueError(f"Error reading file {file_path}: {str(e)}")
        except Exception as e:
            raise ValueError(f"Error reading file {file_path}: {str(e)}")
    
    def generate_java(self, cobol_code: str) -> str:
        """Convert COBOL code to Java using Gemini."""
        prompt = f"""
        Convert this COBOL code to Java. Make it simple and readable.
        Focus on functionality first, then optimize if needed.
        
        Important considerations:
        1. Handle COBOL-specific data types appropriately
        2. Maintain the same business logic
        3. Use modern Java practices
        4. Include proper error handling
        5. DO NOT include any markdown formatting or backticks in the output
        6. Return ONLY the Java code, no explanations or comments
        7. The class name MUST be 'Generated' to match the file name
        
        COBOL Code:
        {cobol_code}
        """
        try:
            response = self.model.generate_content(prompt)
            # Clean up the response by removing any markdown formatting
            java_code = response.text
            # Remove any backticks and markdown code blocks
            java_code = java_code.replace('```java', '').replace('```', '')
            # Remove any leading/trailing whitespace
            java_code = java_code.strip()
            
            # Ensure the class name is 'Generated'
            if 'class ' in java_code:
                # Find the first class declaration
                class_index = java_code.find('class ')
                if class_index != -1:
                    # Get the class name
                    class_name_start = class_index + 6
                    class_name_end = java_code.find('{', class_name_start)
                    if class_name_end != -1:
                        current_class = java_code[class_name_start:class_name_end].strip()
                        # Replace the class name with 'Generated'
                        java_code = java_code.replace(f'class {current_class}', 'class Generated')
            
            return java_code
        except Exception as e:
            print(f"Error generating Java code: {str(e)}")
            return ""
    
    def fix_java(self, java_code: str, error: str) -> str:
        """Fix Java code based on compilation error."""
        prompt = f"""
        Fix this Java code. The error is: {error}
        
        Important:
        1. Maintain the original business logic
        2. Fix only the specific error mentioned
        3. Keep the code clean and readable
        
        Java Code:
        {java_code}
        """
        try:
            response = self.model.generate_content(prompt)
            return response.text
        except Exception as e:
            print(f"Error fixing Java code: {str(e)}")
            return java_code 