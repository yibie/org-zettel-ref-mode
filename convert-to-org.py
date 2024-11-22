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
from typing import Tuple, List, Optional
from pathlib import Path
from shutil import which
import importlib
import importlib.util
import unicodedata

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

EBOOK_CONVERT_PATH = which('ebook-convert')

def install_dependencies():
    """安装必要的依赖包"""
    required_packages = {
        'PyMuPDF': 'pymupdf',
        'pdfplumber': 'pdfplumber',
        'PyPDF2': 'PyPDF2',
        'html2text': 'html2text',
        'ebooklib': 'ebooklib',
        'beautifulsoup4': 'beautifulsoup4'
    }
    
    logger.info("Checking and installing required packages...")
    
    for package_name, pip_name in required_packages.items():
        try:
            __import__(package_name.lower())
            logger.info(f"{package_name} is already installed")
        except ImportError:
            logger.info(f"Installing {package_name}...")
            try:
                subprocess.check_call([sys.executable, "-m", "pip", "install", pip_name])
                logger.info(f"Successfully installed {package_name}")
            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to install {package_name}: {e}")


install_dependencies()


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
    """转换 Markdown 到 Org 格式"""
    success = convert_with_pandoc(input_file, output_file, 'markdown')
    if success:
        post_process_org(output_file)
    return success

def convert_html(input_file: Path, output_file: Path) -> bool:
    """转换 HTML 到 Org 格式"""
    success = convert_with_pandoc(input_file, output_file, 'html')
    if success:
        post_process_org(output_file)
    return success

def convert_epub(input_file: Path, output_file: Path) -> bool:
    """转换 EPUB 到 Org 格式，包括图片提取"""
    try:
        # 创建图片保存目录
        images_dir = output_file.parent / f"{output_file.stem}_images"
        images_dir.mkdir(parents=True, exist_ok=True)
        
        # 使用 pandoc 转换，添加图片提取选项
        cmd = ['pandoc'] + PANDOC_ORG_OPTS + [
            '--extract-media=' + str(images_dir),  # 提取媒体文件
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
            # 处理转换后的文件，更新图片链接
            content = output_file.read_text(encoding='utf-8')
            
            # 更新图片链接为相对路径
            content = re.sub(
                r'\[\[file:(.*?)\]\]',
                lambda m: f'[[file:{images_dir.name}/{os.path.basename(m.group(1))}]]',
                content
            )
            
            # 保存更新后的内容
            output_file.write_text(content, encoding='utf-8')
            
            # 后处理
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
                logging.info(f"Successfully converted and archived: {file_path.name}")
                
            except PermissionError as e:
                logging.error(f"Permission denied when archiving file: {e}")
                # If archiving fails, but conversion is successful, return True
                return True
            except Exception as e:
                logging.error(f"Error archiving file: {e}")
                return True
                
            return True
        
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

# Pandoc conversion options
PANDOC_ORG_OPTS = [
    '--wrap=none',      # Avoid automatic line wrapping
    '--standalone', # Create complete document
    '-t', 'org', # Output file type is org 
    '--no-highlight',
    '--extract-media=.', # 默认图片提取选项
]

def convert_with_pandoc(input_file: Path, output_file: Path, input_format: str) -> bool:
    """Convert file to org format using pandoc"""
    cmd = ['pandoc'] + PANDOC_ORG_OPTS + [
        '-f', input_format,
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
        return True
    except subprocess.CalledProcessError as e:
        logging.error(f"Pandoc conversion failed for {input_file}: {e.stderr}")
        return False

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