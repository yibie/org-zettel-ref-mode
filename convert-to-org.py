import os
import sys
import re
import argparse
import subprocess
import platform
import zipfile
import tempfile
import shutil
import logging
import venv
import time
from PyPDF2 import PdfReader



# Setting Log 
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')




# Setting DEFAULT path
DEFAULT_TEMP_FOLDER = os.path.expanduser("~/Documents/temp_convert/")
DEFAULT_REFERENCE_FOLDER = os.path.expanduser("~/Documents/ref/")
DEFAULT_ARCHIVE_FOLDER = os.path.expanduser("/Volumes/Collect/archives/")

def find_conda():
    conda_path = shutil.which('conda')
    if conda_path:
        return conda_path
    common_paths = [
        os.path.expanduser('~/miniconda3/bin/conda'),
        os.path.expanduser('~/anaconda3/bin/conda'),
        '/usr/local/bin/conda',
        '/opt/conda/bin/conda'
    ]
    for path in common_paths:
        if os.path.exists(path):
            return path
    return None

def is_venv():
    return (hasattr(sys, 'real_prefix') or
            (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix))

def activate_environment():
    if 'CONDA_PREFIX' in os.environ:
        conda_path = find_conda()
        if conda_path:
            # First run conda init
            init_command = f"{conda_path} init bash"
            subprocess.run(init_command, shell=True, capture_output=True, text=True)
            
            # Try to active env
            activate_command = f"source $(dirname $(dirname {conda_path}))/etc/profile.d/conda.sh && conda activate {os.environ.get('CONDA_DEFAULT_ENV', 'base')}"
            result = subprocess.run(activate_command, shell=True, capture_output=True, text=True, executable='/bin/bash')
            print(f"Activation command output: {result.stdout}")
            print(f"Activation command error: {result.stderr}")
            if result.returncode == 0:
                print(f"Activated Conda environment: {os.environ.get('CONDA_DEFAULT_ENV', 'base')}")
            else:
                print(f"Failed to activate Conda environment. Return code: {result.returncode}")
        else:
            print("Conda not found, but CONDA_PREFIX is set. Using current environment.")
    elif is_venv():
        print(f"Using venv: {sys.prefix}")
    else:
        print("No virtual environment detected. Using system Python.")

    # 打印 Python 路径和版本信息，以便调试
    print(f"Python executable: {sys.executable}")
    print(f"Python version: {sys.version}")

# 在脚本开始时调用此函数
activate_environment()

# Constants
MAX_PDF_SIZE_MB = 100
IMAGES_FOLDER = os.path.expanduser("~/Documents/ref/images")
TIMEOUT_SECONDS = 300  # 5 minutes timeout

def is_pdf_processable(file_path):
    return os.path.getsize(file_path) / (1024 * 1024) <= MAX_PDF_SIZE_MB

def run_with_timeout(cmd, timeout_sec):
    if platform.system() != 'Windows':
        full_cmd = ['timeout', str(timeout_sec)] + cmd
        return subprocess.run(full_cmd, capture_output=True, text=True)
    else:
        try:
            return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout_sec)
        except subprocess.TimeoutExpired:
            return subprocess.CompletedProcess(cmd, -1, "", "Process timed out")

def is_valid_epub(epub_path):
    try:
        with zipfile.ZipFile(epub_path, 'r') as zip_ref:
            return True
    except zipfile.BadZipFile:
        return False

def preprocess_epub(epub_path):
    if not is_valid_epub(epub_path):
        print(f"Warning: {epub_path} is not a valid EPUB file. Skipping preprocessing.")
        return epub_path

    temp_dir = tempfile.mkdtemp()
    try:
        with zipfile.ZipFile(epub_path, 'r') as zip_ref:
            zip_ref.extractall(temp_dir)

        container_path = os.path.join(temp_dir, 'META-INF', 'container.xml')
        if os.path.exists(container_path):
            with open(container_path, 'r', encoding='utf-8') as f:
                content = f.read()
            content = content.replace('../', '')
            with open(container_path, 'w', encoding='utf-8') as f:
                f.write(content)

        new_epub_path = epub_path + '.fixed.epub'
        with zipfile.ZipFile(new_epub_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
            for root, _, files in os.walk(temp_dir):
                for file in files:
                    file_path = os.path.join(root, file)
                    arcname = os.path.relpath(file_path, temp_dir)
                    zipf.write(file_path, arcname)
        return new_epub_path
    except Exception as e:
        print(f"Error preprocessing EPUB {epub_path}: {str(e)}")
        return epub_path
    finally:
        shutil.rmtree(temp_dir)

def find_calibre_ebook_convert():
    """Attempt to find the ebook-convert executable from Calibre"""
    possible_paths = [
        "/Applications/calibre.app/Contents/MacOS/ebook-convert",  # macOS
        "C:\\Program Files\\Calibre2\\ebook-convert.exe",          # Windows
        "C:\\Program Files (x86)\\Calibre2\\ebook-convert.exe",    # Windows 32-bit on 64-bit
        "/usr/bin/ebook-convert",                                  # Linux
    ]

    for path in possible_paths:
        if os.path.isfile(path):
            return path

    # If not found in common locations, try to find it in PATH
    try:
        return shutil.which("ebook-convert")
    except:
        return None

EBOOK_CONVERT_PATH = find_calibre_ebook_convert()

def convert_epub_to_text(input_file, output_file):
    try:
        # First, try using pandoc
        subprocess.run(['pandoc', '-f', 'epub', '-t', 'plain', '-o', output_file, input_file],
                       check=True, capture_output=True, text=True)
        print(f"Converted EPUB to text using pandoc: {input_file} -> {output_file}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error converting EPUB to text with pandoc {input_file}: {e.stderr}")
        # If pandoc fails, try using ebook-convert from Calibre
        if EBOOK_CONVERT_PATH:
            try:
                subprocess.run([EBOOK_CONVERT_PATH, input_file, output_file],
                               check=True, capture_output=True, text=True)
                print(f"Converted EPUB to text using ebook-convert: {input_file} -> {output_file}")
                return True
            except subprocess.CalledProcessError as e:
                print(f"Error converting EPUB to text with ebook-convert {input_file}: {e.stderr}")
        else:
            print("ebook-convert not found. Please make sure Calibre is installed and added to PATH.")
    except FileNotFoundError:
        print("pandoc not found. Please install pandoc.")
    return False

def convert_with_pandoc(input_file, output_file):
    os.makedirs(IMAGES_FOLDER, exist_ok=True)

    if input_file.lower().endswith('.epub'):
        input_file = preprocess_epub(input_file)

    cmd = [
        'pandoc',
        input_file,
        '-o', output_file,
        f'--extract-media={IMAGES_FOLDER}',
        '--wrap=none',
        f'--resource-path={IMAGES_FOLDER}'
    ]
    result = run_with_timeout(cmd, TIMEOUT_SECONDS)

    if input_file.endswith('.fixed.epub'):
        os.remove(input_file)

    if result.returncode == 0:
        print(f"Converted: {input_file} -> {output_file}")
        print(f"Extracted images (if any) to: {IMAGES_FOLDER}")
        return True
    else:
        print(f"Error converting {input_file}: {result.stderr}")
        if input_file.lower().endswith('.epub'):
            print("Attempting fallback conversion for EPUB...")
            text_file = output_file.rsplit('.', 1)[0] + '.txt'
            if convert_epub_to_text(input_file, text_file):
                return convert_with_pandoc(text_file, output_file)
        return False

def convert_pdf_to_text(input_file, output_file):
    try:
        subprocess.run(['pdftotext', input_file, output_file], check=True, capture_output=True, text=True)
        print(f"Converted PDF to text: {input_file} -> {output_file}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error converting PDF to text {input_file}: {e.stderr}")
        return False
    except FileNotFoundError:
        print("pdftotext not found. Please install poppler-utils.")
        return False

def is_scanned_pdf(file_path):
    """
    Determines if a PDF file is scanned or digital.

    Args:
    file_path (str): Path to the PDF file.

    Returns:
    bool: True if the PDF is likely scanned, False if it's likely digital.
    """
    try:
        with open(file_path, 'rb') as file:
            reader = PdfReader(file)
            page = reader.pages[0]  # Check only the first page

            # If the page has text, it's likely a digital PDF
            if page.extract_text().strip():
                return False

            # If the page has images and no text, it's likely a scanned PDF
            if '/XObject' in page['/Resources']:
                return True

        # If we can't determine, assume it's digital
        return False
    except Exception as e:
        logging.error(f"Error processing PDF {file_path}: {str(e)}")
        return False  # Assume digital if we can't process the file

def ocr_pdf(input_file, output_file):
    try:
        # Convert PDF to images
        images = convert_from_path(input_file)

        # Perform OCR on each image
        text = ""
        for i, image in enumerate(images):
            print(f"Processing page {i+1}/{len(images)}...")
            # Use both English and Chinese (simplified and traditional) for OCR
            page_text = pytesseract.image_to_string(image, lang='eng+chi_sim+chi_tra')
            text += f"\n\n--- Page {i+1} ---\n\n" + page_text

        # Write the extracted text to the output file
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(text)

        print(f"OCR processing complete for scanned PDF: {input_file} -> {output_file}")
        return True
    except Exception as e:
        print(f"Error during OCR processing of PDF {input_file}: {str(e)}")
        return False

def process_pdf(input_file, output_file):
    if is_scanned_pdf(input_file):
        temp_txt_file = output_file.rsplit('.', 1)[0] + '.txt'
        success = ocr_pdf(input_file, temp_txt_file)
        if success:
            return convert_with_pandoc(temp_txt_file, output_file)
        return False
    else:
        return convert_pdf_to_text(input_file, output_file)

def convert_mobi_to_text(input_file, output_file):
    """
    Convert MOBI file to text using Calibre's ebook-convert tool.
    """
    if not EBOOK_CONVERT_PATH:
        print("ebook-convert not found. Please install Calibre.")
        return False

    try:
        subprocess.run([EBOOK_CONVERT_PATH, input_file, output_file],
                       check=True, capture_output=True, text=True)
        print(f"Converted MOBI to text: {input_file} -> {output_file}")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error converting MOBI to text {input_file}: {e.stderr}")
        return False

def process_file(file, temp_folder, reference_folder, archive_folder):
    input_file = os.path.join(temp_folder, file)
    file_extension = os.path.splitext(file)[1].lower()
    output_name = os.path.splitext(file)[0]  # No sanitization

    if file_extension in ['.md', '.html', '.epub', '.txt', '.mobi']:
        if file_extension == '.mobi':
            # First convert MOBI to text
            text_file = os.path.join(temp_folder, f"{output_name}.txt")
            success = convert_mobi_to_text(input_file, text_file)
            if success:
                # Then convert text to org
                output_file = os.path.join(reference_folder, f"{output_name}.org")
                success = convert_with_pandoc(text_file, output_file)
                os.remove(text_file)  # Remove temporary text file
            else:
                return False
        else:
            output_file = os.path.join(reference_folder, f"{output_name}.org")
            success = convert_with_pandoc(input_file, output_file)
    elif file_extension == '.pdf':
        if is_pdf_processable(input_file):
            output_file = os.path.join(reference_folder, f"{output_name}.org")
            success = process_pdf(input_file, output_file)
        else:
            print(f"Skipping oversized PDF file: {file}")
            return False
    else:
        print(f"Skipping unsupported file: {file}")
        return False

    if success:
        archive_path = os.path.join(archive_folder, file)
        shutil.move(input_file, archive_path)
        print(f"Archived: {input_file} -> {archive_path}")
        return True
    else:
        print(f"Failed to process: {file}")
        return False

def process_files_by_type(temp_folder, reference_folder, archive_folder):
    unprocessed_files = []

    file_types = ['.md', '.html', '.epub', '.pdf', '.txt', '.mobi']

    for file_type in file_types:
        print(f"\nProcessing {file_type.upper()} files...")
        files = [f for f in os.listdir(temp_folder) if f.lower().endswith(file_type)]

        for file in files:
            start_time = time.time()
            success = process_file(file, temp_folder, reference_folder, archive_folder)
            end_time = time.time()

            if not success or (end_time - start_time) >= TIMEOUT_SECONDS:
                unprocessed_files.append(file)

        print(f"Finished processing {file_type.upper()} files.")

    return unprocessed_files

def check_folders(temp_folder, reference_folder, archive_folder):
    for folder in [temp_folder, reference_folder, archive_folder]:
        if not os.path.exists(folder):
            os.makedirs(folder)
            logging.info(f"Created folder: {folder}")
        elif not os.access(folder, os.W_OK):
            logging.error(f"No write permission for folder: {folder}")
            sys.exit(1)

def create_venv(venv_dir):
    print(f"Creating virtual environment in {venv_dir}")
    venv.create(venv_dir, with_pip=True)

def activate_venv(venv_dir):
    if platform.system() == 'Windows':
        activate_python = os.path.join(venv_dir, 'Scripts', 'python.exe')
    else:
        activate_python = os.path.join(venv_dir, 'bin', 'python')
    
    if not os.path.exists(activate_python):
        print(f"Python executable not found in venv: {activate_python}")
        return False
    
    print(f"Re-running script with venv Python: {activate_python}")
    os.execl(activate_python, activate_python, *sys.argv)
    return True

def install_dependencies(requirements_file):
    print("Installing dependencies...")
    subprocess.check_call([sys.executable, "-m", "pip", "install", "-r", requirements_file])

def setup_environment():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    venv_dir = os.path.join(script_dir, 'venv')
    requirements_file = os.path.join(script_dir, 'requirements.txt')

    # 检查环境设置
    env_setting = os.environ.get('ORG_ZETTEL_REF_PYTHON_ENVIRONMENT', 'system')

    if env_setting == 'system':
        print("Using system Python as specified in settings.")
        return

    # 检查是否在 Conda 环境中
    if 'CONDA_PREFIX' in os.environ:
        print("Running in Conda environment:", os.environ['CONDA_PREFIX'])
        return

    # 检查是否有 venv 或 virtualenv
    if hasattr(sys, 'real_prefix') or (hasattr(sys, 'base_prefix') and sys.base_prefix != sys.prefix):
        print("Running in virtualenv or venv")
        return

    # 如果设置为 'venv' 且没有虚拟环境，创建并激活一个
    if env_setting == 'venv':
        if not os.path.exists(venv_dir):
            create_venv(venv_dir)
        if not activate_venv(venv_dir):
            print("Failed to activate venv. Using system Python.")
            return

    # 检查并安装依赖
    if os.path.exists(requirements_file):
        install_dependencies(requirements_file)
    else:
        print("No requirements.txt file found. Skipping dependency check.")

def main():
    # 在 main 中导入模块，确保在环境设置和依赖安装之后
    import PyPDF2
    from pdf2image import convert_from_path
    import pytesseract
    from PIL import Image

    parser = argparse.ArgumentParser(description="Convert files to Org format")
    parser.add_argument("--temp", help="Temporary folder path", default=DEFAULT_TEMP_FOLDER)
    parser.add_argument("--reference", help="Reference folder path", default=DEFAULT_REFERENCE_FOLDER)
    parser.add_argument("--archive", help="Archive folder path", default=DEFAULT_ARCHIVE_FOLDER)
    
    args = parser.parse_args()
    
    # 使用命令行参数或默认值
    temp_folder = args.temp
    reference_folder = args.reference
    archive_folder = args.archive
    
    print(f"Using folders:")
    print(f"Temporary folder: {temp_folder}")
    print(f"Reference folder: {reference_folder}")
    print(f"Archive folder: {archive_folder}")
    
    # 检查文件夹是否存在，如果不存在则创建
    for folder in [temp_folder, reference_folder, archive_folder]:
        if not os.path.exists(folder):
            try:
                os.makedirs(folder)
                print(f"Created folder: {folder}")
            except OSError as e:
                print(f"Error creating folder {folder}: {e}")
                print("Please manually create the folder or modify the path in the script.")
                sys.exit(1)
    
    # 处理文件
    logging.info("Starting file processing")
    unprocessed_files = process_files_by_type(temp_folder, reference_folder, archive_folder)
    logging.info("File processing completed")

    print("\nProcessing complete.")

    if unprocessed_files:
        print("\nUnprocessed files report:")
        for file in unprocessed_files:
            print(f"- {file}")
        print(f"\nTotal unprocessed files: {len(unprocessed_files)}")
    else:
        print("\nAll files processed successfully.")

if __name__ == "__main__":
    setup_environment()
    main()
