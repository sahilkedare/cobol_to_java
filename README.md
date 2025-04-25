# COBOL to Java Modernization Tool

An AI-powered tool that converts COBOL code to modern Java using LangGraph and Gemini. This tool helps in modernizing legacy COBOL applications by automatically generating equivalent Java code.

## Features

- Converts COBOL code to Java
- Supports multiple COBOL file extensions (.cbl, .cob, .cobol, .cpy)
- Validates generated Java code
- Compares with expected Java output
- Iterative improvement with up to 7 attempts
- Automatic error fixing
- Saves generated Java code to files

## Architecture

### 1. Agentic Workflow via LangGraph
LangGraph orchestrates a graph of specialized agents.

**Enables:**
- Dynamic flow control
- Iterative revision loops
- Tool/function invocation
- Retry logic with feedback

Each node = agent (e.g., converter, validator, fixer).

Each edge = flow logic (success/failure path).

### 2. Iterative Feedback & Revision Loop
After initial conversion, Java code is compiled and validated.

**If issues are found:**
- Enters "Fix & Retry" loop (up to 7 times)
- Fixer agent revises only faulty parts using contextual feedback
- Mimics reflection + recursion for self-improving code generation

### 3. Architectural Advantages
- **Agentic Modularity** – Isolated, reusable components
- **Self-Healing Workflow** – Auto-fixes code until successful
- **Reflection-Driven Reasoning** – Uses compiler/runtime feedback for validation
- **Extensible Design** – Easy to plug in tools like test generators or static analyzers

## Prerequisites

- Python 3.8 or higher
- Java Development Kit (JDK)
- Google Gemini API key

## Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd cobol-java
```

2. Create and activate a virtual environment:
```bash
python -m venv venv
# On Windows
venv\Scripts\activate
# On Unix or MacOS
source venv/bin/activate
```

3. Install dependencies:
```bash
pip install -r requirements.txt
```

4. Create a `.env` file in the project root and add your Gemini API key:
```
GEMINI_API_KEY=your_api_key_here
```

## Usage

Basic usage:
```bash
python main.py <cobol_file> [--expected-java <expected_java_file>]
```

Example:
```bash
python main.py sample.cbl --expected-java expected.java
```

### Arguments

- `cobol_file`: Path to the COBOL file to convert
- `--expected-java`: (Optional) Path to the expected Java file for comparison

### Output

- Generated Java code is saved in the `generated_java` directory
- The output filename matches the input COBOL filename with a `.java` extension
- Logs are displayed in the console showing the conversion progress

## Project Structure

```
.
├── main.py              # Main application entry point
├── cobol_processor.py   # COBOL to Java conversion logic
├── java_validator.py    # Java code validation and comparison
├── workflow.py          # LangGraph workflow definition
├── requirements.txt     # Python dependencies
├── .env                 # Environment variables (not in git)
└── generated_java/      # Output directory for generated Java files
```

## Error Handling

The tool includes comprehensive error handling:
- Validates COBOL file extensions
- Checks for file existence
- Handles compilation errors
- Provides detailed error messages
- Attempts to fix errors automatically

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Uses Google's Gemini AI for code generation
- Built with LangGraph for workflow management
- Inspired by the need to modernize legacy COBOL systems 