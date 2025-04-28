# COBOL/EASYTRIEVE to Java Modernization Tool

An AI-powered tool that converts COBOL and EASYTRIEVE code to modern Java using LangGraph and Gemini. This tool helps in modernizing legacy mainframe applications by automatically generating equivalent Java code.

## Features

- Converts COBOL and EASYTRIEVE code to Java
- Supports multiple file extensions:
  - COBOL: .cbl, .cob, .cobol, .cpy
  - EASYTRIEVE: .ezr, .ez
  - Text files: .txt (auto-detects content type)
- Validates generated Java code
- Compares with expected Java output
- Iterative improvement with up to 7 attempts
- Automatic error fixing
- Saves generated Java code to files
- **LangSmith Tracing** - Monitors and traces the entire conversion workflow
- **Web Interface** - Beautiful Streamlit UI for easy interaction

## Architecture

### 1. Agentic Workflow via LangGraph
LangGraph orchestrates a graph of specialized agents.

**Enables:**
- Dynamic flow control
- Iterative revision loops
- Tool/function invocation
- Retry logic with feedback
- **Workflow Tracing** - Track each step of the conversion process

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
- **Observability** – LangSmith tracing for monitoring and debugging

## Prerequisites

- Python 3.8 or higher
- Java Development Kit (JDK)
- Google Gemini API key
- LangSmith API key (for tracing)

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

4. Create a `.env` file in the project root and add your API keys:
```
GOOGLE_API_KEY=your_gemini_api_key_here
LANGSMITH_API_KEY=your_langsmith_api_key_here
LANGSMITH_TRACING=true
LANGSMITH_ENDPOINT=https://api.smith.langchain.com
```

## Usage Guide

### Web Interface (Streamlit)

The easiest way to use the tool is through the Streamlit web interface:

```bash
# Run the Streamlit app
streamlit run app.py
```

The web interface provides:
- File upload for COBOL/EASYTRIEVE code
- Optional upload of expected Java output
- Real-time conversion status
- Side-by-side view of input and generated code
- Beautiful, centered UI for better readability

### Command Line Interface

#### Command Syntax
```bash
python main.py <input_file> [--expected-java <expected_java_file>]
```

#### Arguments
- `input_file`: Path to the input file (supports COBOL and EASYTRIEVE extensions)
- `--expected-java`: (Optional) Path to the expected Java output file for comparison

#### Example Usage
```bash
# COBOL conversion
python main.py program.cbl

# EASYTRIEVE conversion
python main.py report.ezr

# Conversion with expected output comparison
python main.py program.cbl --expected-java expected.java

# Using a text file (auto-detects content type)
python main.py code.txt
```

### Output

- Generated Java code is saved in the `generated_java` directory
- Conversion traces can be viewed in the LangSmith dashboard

### Tracing with LangSmith

The tool uses LangSmith for tracing the entire conversion workflow. This provides:

- **Step-by-step monitoring** of the conversion process
- **Performance metrics** for each conversion step
- **Error tracking** and debugging information
- **Workflow visualization** in the LangSmith dashboard

To view traces:
1. Run your conversion as usual
2. Go to the [LangSmith Dashboard](https://smith.langchain.com/)
3. Navigate to the "Traces" section
4. Find your conversion run and explore the detailed trace