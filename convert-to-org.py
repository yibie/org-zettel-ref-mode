import warnings
warnings.filterwarnings("ignore", message=".*tqdm.*")


import os
import sys
import re
import logging
import shutil
import argparse
import time
import subprocess
import email
from email import policy
from email.parser import BytesParser
from datetime import datetime
from typing import Tuple, List, Optional
from pathlib import Path
from shutil import which
import importlib
import importlib.util
import unicodedata
import tempfile

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

import warnings
warnings.filterwarnings("ignore", message=".*tqdm.*")

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def read_requirements(requirements_file: Path) -> list:
    """Read the contents of the requirements.txt file"""
    try:
        with open(requirements_file, 'r') as f:
            return [line.strip() for line in f if line.strip() and not line.startswith('#')]
    except Exception as e:
        logger.error(f"Reading requirements.txt failed: {e}")
        return []

def check_package_installed(package: str, python_path: str) -> bool:
    """Check if the package is installed"""
    try:
        cmd = [python_path, "-c", f"import {package.split('==')[0]}"]
        result = subprocess.run(cmd, capture_output=True, text=True)
        return result.returncode == 0
    except Exception:
        return False

def install_requirements(pip_path: str, requirements: list) -> bool:
    """Install the required packages"""
    try:
        for package in requirements:
            logger.info(f"Installing {package}...")
            subprocess.run([pip_path, "install", package], check=True)
        return True
    except subprocess.CalledProcessError as e:
        logger.error(f"安装包失败: {e}")
        return False

def setup_environment():
    """Automatically set up and manage the virtual environment"""
    script_dir = Path(__file__).parent.absolute()
    requirements_file = script_dir / "requirements.txt"
    venv_type = os.environ.get('ORG_ZETTEL_REF_PYTHON_ENV', 'venv')
    venv_name = 'org-zettel-ref-env'

    # 读取 requirements.txt
    if not requirements_file.exists():
        logger.error("requirements.txt 文件不存在")
        return False
    
    requirements = read_requirements(requirements_file)
    if not requirements:
        logger.error("Can not read the requirements list")
        return False

    if venv_type == 'conda':
        return setup_conda_env(venv_name, requirements)
    else:
        venv_path = script_dir / '.venv'
        return setup_venv_env(venv_path, requirements)

def setup_venv_env(venv_path: Path, requirements: list) -> bool:
    """Set up and manage the venv virtual environment"""
    try:
        # Determine the path to the executable file in the virtual environment
        if os.name == 'nt':  # Windows
            bin_dir = venv_path / 'Scripts'
            python_path = str(bin_dir / 'python.exe')
            pip_path = str(bin_dir / 'pip.exe')
        else:  # Unix-like
            bin_dir = venv_path / 'bin'
            python_path = str(bin_dir / 'python')
            pip_path = str(bin_dir / 'pip')

        # If the virtual environment does not exist, create it
        if not venv_path.exists():
            logger.info("Creating new venv virtual environment...")
            try:
                subprocess.run([sys.executable, "-m", "venv", str(venv_path)], 
                             check=True, 
                             capture_output=True,
                             text=True)
                logger.info("Virtual environment created successfully")
            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to create virtual environment: {e.stderr}")
                return False

        # 验证虚拟环境是否正确创建
        if not Path(python_path).exists():
            logger.error(f"Python executable not found at {python_path}")
            return False
            
        if not Path(pip_path).exists():
            logger.error(f"Pip executable not found at {pip_path}")
            # 尝试安装 pip
            try:
                subprocess.run([python_path, "-m", "ensurepip"], 
                             check=True,
                             capture_output=True,
                             text=True)
                logger.info("Pip installed successfully")
            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to install pip: {e.stderr}")
                return False

        # 检查和安装依赖
        logger.info("Checking dependencies...")
        missing_packages = []
        for package in requirements:
            package_name = package.split('==')[0]
            if not check_package_installed(package_name, python_path):
                missing_packages.append(package)

        if missing_packages:
            logger.info(f"Missing packages: {', '.join(missing_packages)}")
            if not install_requirements(pip_path, missing_packages):
                return False
            logger.info("All dependencies installed")
        else:
            logger.info("All dependencies already installed")

        # 激活虚拟环境
        os.environ['VIRTUAL_ENV'] = str(venv_path)
        os.environ['PATH'] = str(bin_dir) + os.pathsep + os.environ['PATH']
        sys.executable = python_path
        
        return True

    except Exception as e:
        logger.error(f"Setting up venv environment failed: {str(e)}")
        return False

def setup_conda_env(env_name: str, requirements: list) -> bool:
    """Set up and manage the conda virtual environment"""
    try:
        # Check if conda is available
        if not shutil.which('conda'):
            logger.error("Conda command not found")
            return False

        # Check if the environment exists
        result = subprocess.run(["conda", "env", "list"], capture_output=True, text=True)
        env_exists = env_name in result.stdout

        if not env_exists:
            logger.info(f"Creating new conda environment: {env_name}")
            subprocess.run(["conda", "create", "-n", env_name, "python", "-y"], check=True)

        # Get the Python and pip paths for the environment
        conda_prefix = subprocess.check_output(["conda", "info", "--base"]).decode().strip()
        env_path = os.path.join(conda_prefix, "envs", env_name)
        
        if os.name == 'nt':  # Windows
            python_path = os.path.join(env_path, "python.exe")
            pip_path = os.path.join(env_path, "Scripts", "pip.exe")
        else:  # Unix-like
            python_path = os.path.join(env_path, "bin", "python")
            pip_path = os.path.join(env_path, "bin", "pip")

        # Check and install dependencies
        logger.info("Checking dependencies...")
        missing_packages = []
        for package in requirements:
            package_name = package.split('==')[0]
            if not check_package_installed(package_name, python_path):
                missing_packages.append(package)

        if missing_packages:
            logger.info(f"Missing packages: {', '.join(missing_packages)}")
            cmd = ["conda", "run", "-n", env_name, "pip", "install"] + missing_packages
            subprocess.run(cmd, check=True)
            logger.info("All dependencies installed")
        else:
            logger.info("All dependencies installed")

        # Activate the environment
        os.environ['CONDA_DEFAULT_ENV'] = env_name
        os.environ['CONDA_PREFIX'] = env_path
        
        return True

    except Exception as e:
        logger.error(f"Setting up conda environment failed: {e}")
        return False


if not setup_environment():
    logger.error("Virtual environment setup failed, exiting...")
    sys.exit(1)

AVAILABLE_PDF_PROCESSORS = []

try:
    import fitz  # PyMuPDF
    AVAILABLE_PDF_PROCESSORS.append('pymupdf')
except ImportError:
    logger.warning("PyMuPDF (fitz) not found. Some PDF processing features may be limited.")

try:
    import pdfplumber
    AVAILABLE_PDF_PROCESSORS.append('pdfplumber')
except ImportError:
    logger.warning("pdfplumber not found. Some PDF processing features may be limited.")

try:
    from PyPDF2 import PdfReader
except ImportError:
    logger.warning("PyPDF2 not found. PDF processing may be limited.")

def sanitize_filename(filename: str) -> str:
    """Clean up unsafe characters in filenames"""
    # Replace Windows/Unix unsupported characters
    invalid_chars = r'[<>:"/\\|?*\[\]]'
    filename = re.sub(invalid_chars, '_', filename)
    # Handle consecutive underscores
    filename = re.sub(r'_+', '_', filename)
    return filename.strip('_')

def convert_markdown(input_file: Path, output_file: Path) -> bool:
    """Convert Markdown to Org format"""
    # Create image directory for this conversion
    images_dir = output_file.parent / f"{output_file.stem}_images"
    images_dir.mkdir(parents=True, exist_ok=True)
    
    cmd = ['pandoc',
        '--wrap=none',
        '--standalone',
        '-t', 'org',
        '--no-highlight',
        f'--extract-media={images_dir}',  # 指定图片保存目录
        '-f', 'markdown',
        str(input_file),
        '-o', str(output_file)
    ]
    
    try:
        result = subprocess.run(
            cmd,
            check=True,
            capture_output=True,
            text=True
        )
        
        if result.returncode == 0:
            # Update image links in the converted file
            content = output_file.read_text(encoding='utf-8')
            content = re.sub(
                r'\[\[file:(.*?)\]\]',
                lambda m: f'[[file:{images_dir.name}/{os.path.basename(m.group(1))}]]',
                content
            )
            output_file.write_text(content, encoding='utf-8')
            
            post_process_org(output_file)
            return True
            
        return False
    except Exception as e:
        logging.error(f"Pandoc conversion failed for {input_file}: {str(e)}")
        return False

def convert_html(input_file: Path, output_file: Path) -> bool:
    """Convert HTML to Org format"""
    # Create image directory for this conversion
    images_dir = output_file.parent / f"{output_file.stem}_images"
    images_dir.mkdir(parents=True, exist_ok=True)
    
    cmd = ['pandoc',
        '--wrap=none',
        '--standalone',
        '-t', 'org',
        '--no-highlight',
        f'--extract-media={images_dir}',  # 指定图片保存目录
        '-f', 'html',
        str(input_file),
        '-o', str(output_file)
    ]
    
    try:
        result = subprocess.run(
            cmd,
            check=True,
            capture_output=True,
            text=True
        )
        
        if result.returncode == 0:
            # Update image links in the converted file
            content = output_file.read_text(encoding='utf-8')
            content = re.sub(
                r'\[\[file:(.*?)\]\]',
                lambda m: f'[[file:{images_dir.name}/{os.path.basename(m.group(1))}]]',
                content
            )
            output_file.write_text(content, encoding='utf-8')
            
            post_process_org(output_file)
            return True
            
        return False
    except Exception as e:
        logging.error(f"Pandoc conversion failed for {input_file}: {str(e)}")
        return False

def convert_epub(input_file: Path, output_file: Path) -> bool:
    """Convert EPUB to Org format, including image extraction"""
    try:
        # Create image save directory with output file's name
        images_dir = output_file.parent / f"{output_file.stem}_images"
        images_dir.mkdir(parents=True, exist_ok=True)
        
        # Use pandoc to convert, specify image extraction directory
        cmd = ['pandoc',
            '--wrap=none',
            '--standalone',
            '-t', 'org',
            '--no-highlight',
            f'--extract-media={images_dir}',  # 指定图片保存目录
            '-f', 'epub',
            str(input_file),
            '-o', str(output_file)
        ]
        
        result = subprocess.run(
            cmd,
            check=True,
            capture_output=True,
            text=True
        )
        
        if result.returncode == 0:
            # Process the converted file, update image links
            content = output_file.read_text(encoding='utf-8')
            
            # Update image links to relative paths
            content = re.sub(
                r'\[\[file:(.*?)\]\]',
                lambda m: f'[[file:{images_dir.name}/{os.path.basename(m.group(1))}]]',
                content
            )
            
            # Save the updated content
            output_file.write_text(content, encoding='utf-8')
            
            # Post-process
            post_process_org(output_file)
            return True
            
        return False
        
    except Exception as e:
        logging.error(f"Error converting EPUB {input_file}: {str(e)}")
        return False

def convert_text_to_org(input_file: str, output_file: str) -> Tuple[bool, List[str]]:
    """
    Convert text files (including .txt and .rst) to org format
    """
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Add basic conversion for .rst files   
        if input_file.lower().endswith('.rst'):
            # Convert RST title format
            content = re.sub(r'={3,}', lambda m: '* ' * len(m.group()), content)
            content = re.sub(r'-{3,}', lambda m: '* ' * len(m.group()), content)
            
            # Convert RST reference format
            content = re.sub(r'^\.\. code-block::', '#+BEGIN_SRC', content, flags=re.MULTILINE)
            content = re.sub(r'^\.\. note::', '#+BEGIN_NOTE', content, flags=re.MULTILINE)

        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(content)
        return True, []
    except Exception as e:
        return False, [f"Error converting text file: {str(e)}"]

def process_pdf(input_file: str) -> Tuple[Optional[str], List[str]]:
    """Process PDF file content"""
    text = ""
    errors = []

    # PyMuPDF processing method
    if 'pymupdf' in AVAILABLE_PDF_PROCESSORS:
        try:
            with fitz.open(input_file) as doc:
                text = ""
                for page in doc:
                    text += page.get_text()
                if text.strip():
                    return text, []
        except Exception as e:
            errors.append(f"PyMuPDF error: {str(e)}")

    # PyPDF2 processing method
    try:
        from PyPDF2 import PdfReader
        reader = PdfReader(input_file)
        if reader.is_encrypted:
            try:
                reader.decrypt('')
            except:
                errors.append("PDF is encrypted")
                return None, errors

        text = ""
        for page in reader.pages:
            try:
                text += page.extract_text() or ""
            except Exception as e:
                errors.append(f"Error extracting text: {str(e)}")
                continue

        if text.strip():
            return text, []
    except Exception as e:
        errors.append(f"PyPDF2 error: {str(e)}")

    return None, errors

def convert_pdf_to_org(input_file: str, output_file: str) -> Tuple[bool, List[str]]:
    """Convert PDF to Org format"""
    try:
        text, errors = process_pdf(input_file)
        if text:
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(text)
            return True, []
        return False, errors
    except Exception as e:
        return False, [f"Error converting PDF: {str(e)}"]

def process_file(file_path: Path, reference_dir: Path, archive_dir: Path) -> bool:
    """
    Process a single file conversion
    """
    try:
        # Get a safe filename
        safe_name = get_safe_filename(file_path.stem)
        output_file = reference_dir / f"{safe_name}.org"
        
        # Choose conversion method based on file type
        suffix = file_path.suffix.lower()
        success = False
        
        if suffix == '.md':
            success = convert_markdown(file_path, output_file)
        elif suffix == '.html':
            success = convert_html(file_path, output_file)
        elif suffix == '.epub':
            success = convert_epub(file_path, output_file)
        elif suffix == '.eml':
            success = convert_eml(file_path, output_file)
        elif suffix == '.pdf':
            success, errors = convert_pdf_to_org(str(file_path), str(output_file))
            if errors:
                logging.error(f"PDF conversion errors: {errors}")
        else:
            logging.warning(f"Unsupported file type: {suffix}")
            return False
        
        if success:
            try:
                # Make sure the archive directory exists
                archive_dir.mkdir(parents=True, exist_ok=True)
                
                # Check if the target path is writable
                if not os.access(archive_dir, os.W_OK):
                    logging.error(f"No write permission for archive directory: {archive_dir}")
                    return False
                
                # Move the file to the archive directory
                archive_path = archive_dir / file_path.name
                shutil.move(str(file_path), str(archive_path))
                
                # 添加成功转换的详细消息
                logger.info("=" * 50)
                logger.info(f"Successfully converted: {file_path.name}")
                logger.info(f"Output file: {output_file}")
                logger.info(f"Archived to: {archive_path}")
                logger.info("=" * 50)
                
            except PermissionError as e:
                logging.error(f"Permission denied when archiving file: {e}")
                # If archiving fails, but conversion is successful, still show conversion success
                logger.info("=" * 50)
                logger.info(f"Successfully converted: {file_path.name}")
                logger.info(f"Output file: {output_file}")
                logger.info("Note: File archiving failed due to permission error")
                logger.info("=" * 50)
                return True
            except Exception as e:
                logging.error(f"Error archiving file: {e}")
                # If archiving fails, but conversion is successful, still show conversion success
                logger.info("=" * 50)
                logger.info(f"Successfully converted: {file_path.name}")
                logger.info(f"Output file: {output_file}")
                logger.info("Note: File archiving failed")
                logger.info("=" * 50)
                return True
                
            return True
        else:
            logger.error(f"Failed to convert: {file_path.name}")
            return False
        
    except Exception as e:
        logging.error(f"Error processing {file_path}: {str(e)}")
        return False

def check_calibre_installation():
    """Check if Calibre is installed"""
    global EBOOK_CONVERT_PATH
    
    if not EBOOK_CONVERT_PATH:
        logger.warning("Calibre's ebook-convert tool not found.")
        logger.warning("To process MOBI files, please install Calibre:")
        logger.warning("- macOS: brew install calibre")
        logger.warning("- Linux: sudo apt-get install calibre")
        logger.warning("- Windows: Download from https://calibre-ebook.com/download")
        return False
    return True

def check_dependencies():
    """Check if all dependencies are installed"""
    # Check pandoc
    if not shutil.which('pandoc'):
        raise RuntimeError("Pandoc is not installed. Please install pandoc first.")
    
    # Only keep necessary Python package checks
    required_packages = [
        'PyMuPDF',  # Only for PDF processing
    ]
    
    missing_packages = []
    for package in required_packages:
        if importlib.util.find_spec(package.lower()) is None:
            missing_packages.append(package)
    
    if missing_packages:
        packages_str = ' '.join(missing_packages)
        logging.warning(f"Missing packages: {packages_str}")
        logging.info("Attempting to install missing packages...")
        try:
            subprocess.run(['pip', 'install'] + missing_packages, check=True)
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Failed to install required packages: {str(e)}")

def post_process_org(file_path: Path) -> None:
    """Process the converted org file, clean up unnecessary marks"""
    try:
        content = file_path.read_text(encoding='utf-8')
        # Clean up unnecessary line end backslashes
        cleaned = re.sub(r'\\\n', '\n', content)
        # Add other cleanup rules if needed
        file_path.write_text(cleaned, encoding='utf-8')
    except Exception as e:
        logging.error(f"Post-processing failed for {file_path}: {str(e)}")

def get_safe_filename(filename: str) -> str:
    """
    Convert filename to a safe format
    
    Args:
        filename: Original filename
    
    Returns:
        Safe filename    
    """
    # Remove unsafe characters, only keep letters, numbers, Chinese and some basic punctuation
    filename = re.sub(r'[^\w\s\-\.\(\)（）\[\]【】\u4e00-\u9fff]', '_', filename)
    
    # Replace multiple consecutive spaces and underscores with a single underscore
    filename = re.sub(r'[\s_]+', '_', filename)
    
    # Process filename length, truncate if too long (keep the extension)
    max_length = 40  # Set maximum length
    if len(filename) > max_length:
        # Ensure not to truncate in the middle of Chinese characters
        truncated = filename[:max_length]
        # Find the last safe truncation point (underscore or space)
        last_safe = truncated.rfind('_')
        if last_safe > max_length // 2:  # If a suitable truncation point is found
            filename = truncated[:last_safe]
        else:
            filename = truncated
    
        # Remove leading and trailing spaces and underscores
    filename = filename.strip('_').strip()
    
    # Ensure filename is not empty
    if not filename:
        filename = 'unnamed_file'
    
    return filename

def convert_eml(input_file: Path, output_file: Path) -> bool:
    """Convert email (.eml) files to Org format"""
    try:
        # Create image directory for attachments
        images_dir = output_file.parent / f"{output_file.stem}_images"
        images_dir.mkdir(parents=True, exist_ok=True)
        
        # Parse the email file
        with open(input_file, 'rb') as fp:
            msg = BytesParser(policy=policy.default).parse(fp)
        
        # Extract email metadata and content
        subject = msg.get('subject', 'No Subject')
        from_addr = msg.get('from', 'Unknown')
        to_addr = msg.get('to', 'Unknown')
        date_str = msg.get('date', '')
        try:
            date = email.utils.parsedate_to_datetime(date_str)
            date_formatted = date.strftime('%Y-%m-%d %H:%M:%S')
        except:
            date_formatted = date_str
        
        # Start building org content
        org_content = []
        org_content.append(f'#+TITLE: {subject}')
        org_content.append('#+OPTIONS: ^:nil')
        org_content.append(f'#+DATE: {date_formatted}')
        org_content.append('')
        org_content.append('* Email Metadata')
        org_content.append(f'- From: {from_addr}')
        org_content.append(f'- To: {to_addr}')
        org_content.append(f'- Date: {date_formatted}')
        org_content.append('')
        org_content.append('* Content')
        
        # Handle multipart messages
        attachments = []
        
        def extract_content(part):
            content_type = part.get_content_type()
            if content_type == 'text/plain':
                return part.get_content()
            elif content_type == 'text/html':
                # Convert HTML to org using pandoc
                with tempfile.NamedTemporaryFile(mode='w', suffix='.html', delete=False) as temp:
                    temp.write(part.get_content())
                    temp_path = temp.name
                
                try:
                    cmd = [
                        'pandoc',
                        '--wrap=none',
                        '--standalone',
                        '-f', 'html',
                        '-t', 'org',
                        temp_path
                    ]
                    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
                    return result.stdout
                finally:
                    os.unlink(temp_path)
            return None
        
        def process_part(part):
            if part.is_multipart():
                for subpart in part.iter_parts():
                    process_part(subpart)
            else:
                content_type = part.get_content_type()
                if content_type.startswith('text/'):
                    content = extract_content(part)
                    if content:
                        org_content.append(content)
                elif part.get_filename():  # This is an attachment
                    filename = part.get_filename()
                    safe_filename = get_safe_filename(filename)
                    attachment_path = images_dir / safe_filename
                    
                    with open(attachment_path, 'wb') as f:
                        f.write(part.get_payload(decode=True))
                    
                    attachments.append((safe_filename, content_type))
        
        # Process the email content
        if msg.is_multipart():
            process_part(msg)
        else:
            content = extract_content(msg)
            if content:
                org_content.append(content)
        
        # Add attachments section if there are any
        if attachments:
            org_content.append('')
            org_content.append('* Attachments')
            for filename, content_type in attachments:
                org_content.append(f'- [[file:{images_dir.name}/{filename}][{filename}]] ({content_type})')
        
        # Write the org file
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write('\n'.join(org_content))
        
        return True
        
    except Exception as e:
        logging.error(f"Error converting email file {input_file}: {str(e)}")
        return False

def main():
    """Main function"""
    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    
    try:
        # Check dependencies
        check_dependencies()
        
        # Get command line arguments
        parser = argparse.ArgumentParser(description='Convert documents to Org format')
        parser.add_argument('--temp', type=str, required=True, help='Temporary directory path')
        parser.add_argument('--reference', type=str, required=True, help='Reference directory path')
        parser.add_argument('--archive', type=str, required=True, help='Archive directory path')
        
        args = parser.parse_args()
        
        # Convert paths to Path objects
        temp_dir = Path(args.temp)
        reference_dir = Path(args.reference)
        archive_dir = Path(args.archive)
        
        # Ensure directories exist
        for directory in [temp_dir, reference_dir, archive_dir]:
            directory.mkdir(parents=True, exist_ok=True)
        
        # Process files
        for file_path in temp_dir.iterdir():
            print(f"Found file: {file_path}")  
            if file_path.is_file():
                process_file(file_path, reference_dir, archive_dir)
                
    except Exception as e:
        logging.error(f"An error occurred: {str(e)}")
        sys.exit(1)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logger.info("\nProcess interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.error(f"An unexpected error occurred: {e}")
        sys.exit(1)
