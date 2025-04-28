import streamlit as st
import os
from main import run_workflow, get_processor
import tempfile

# Set page config
st.set_page_config(
    page_title="COBOL to Java Converter",
    page_icon="🔄",
    layout="wide"
)

# Custom CSS
st.markdown("""
    <style>
    .stButton>button {
        width: 100%;
        height: 3em;
        background-color: #4CAF50;
        color: white;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        font-size: 16px;
    }
    .stButton>button:hover {
        background-color: #45a049;
    }
    .success-message {
        color: #4CAF50;
        font-weight: bold;
    }
    .error-message {
        color: #f44336;
        font-weight: bold;
    }
    .code-block {
        background-color: #f5f5f5;
        padding: 20px;
        border-radius: 5px;
        margin: 10px 0;
    }
    /* Center all text elements */
    .stMarkdown, .stTitle, .stSubheader, h1, h2, h3, p {
        text-align: center !important;
    }
    /* Center columns */
    .row-widget.stHorizontal {
        justify-content: center;
    }
    /* Center file uploaders */
    .stFileUploader {
        display: flex;
        justify-content: center;
        width: 100%;
    }
    /* Center code blocks */
    .stCodeBlock {
        display: flex;
        justify-content: center;
        width: 100%;
    }
    /* Center spinners and other elements */
    .stSpinner, .element-container {
        display: flex;
        justify-content: center;
        width: 100%;
    }
    </style>
""", unsafe_allow_html=True)

# Title and description
st.title("🔄 COBOL to Java Converter")
st.markdown("""
    This tool helps you convert COBOL code to modern Java. Simply upload your COBOL file and optionally provide an expected Java output file.
    The converter will generate Java code and compare it with your expected output if provided.
""")

# Create two columns for file uploaders
col1, col2 = st.columns(2)

with col1:
    st.subheader("Input Files")
    cobol_file = st.file_uploader("Upload COBOL File", type=['cbl', 'cob', 'cobol', 'cpy', 'txt', 'ezr', 'ez'])
    expected_java = st.file_uploader("Upload Expected Java File (Optional)", type=['java'])

# Generate button
if st.button("Generate Java Code", key="generate"):
    if cobol_file is not None:
        with st.spinner("Converting COBOL to Java..."):
            # Create temporary files for the uploaded content
            with tempfile.NamedTemporaryFile(delete=False, suffix=os.path.splitext(cobol_file.name)[1]) as temp_cobol:
                temp_cobol.write(cobol_file.getvalue())
                temp_cobol_path = temp_cobol.name

            temp_java_path = None
            if expected_java is not None:
                with tempfile.NamedTemporaryFile(delete=False, suffix='.java') as temp_java:
                    temp_java.write(expected_java.getvalue())
                    temp_java_path = temp_java.name

            try:
                # Get the appropriate processor
                processor = get_processor(temp_cobol_path)
                if processor:
                    # Run the workflow
                    result = run_workflow(temp_cobol_path, temp_java_path, processor)
                    
                    if result and "java_code" in result and result["java_code"]:
                        st.success("✅ Java code generated successfully!")
                        
                        # Display the COBOL code
                        st.subheader("Original COBOL Code")
                        st.code(cobol_file.getvalue().decode('utf-8'), language='cobol')
                        
                        # Display the generated Java code
                        st.subheader("Generated Java Code")
                        st.code(result["java_code"], language='java')
                        
                        if result.get("error"):
                            st.warning(f"Note: {result['error']}")
                    else:
                        st.error("Failed to generate Java code")
                else:
                    st.error("Could not determine the appropriate processor for the input file")
            except Exception as e:
                st.error(f"An error occurred: {str(e)}")
            finally:
                # Cleanup temporary files
                os.unlink(temp_cobol_path)
                if temp_java_path:
                    os.unlink(temp_java_path)
    else:
        st.warning("Please upload a COBOL file first")

# Footer
st.markdown("---")
st.markdown("""
    <div style='text-align: center'>
        <p>Made with ❤️ using Streamlit</p>
    </div>
""", unsafe_allow_html=True)