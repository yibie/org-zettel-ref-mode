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

def convert_markdown_to_org(input_file: str, output_file: str) -> Tuple[bool, List[str]]:
    """Convert Markdown to Org format"""
    try:
        with open(input_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Basic Markdown to Org format conversion
        # Convert titles
        content = re.sub(r'^#{1,6}\s+', '*' * 1, content, flags=re.MULTILINE)
        # Convert lists
        content = re.sub(r'^\s*[-*+]\s+', '- ', content, flags=re.MULTILINE)
        # Convert code blocks
        content = re.sub(r'```(\w*)\n', '#+BEGIN_SRC \\1\n', content)
        content = re.sub(r'```', '#+END_SRC', content)
        
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(content)
        return True, []
    except Exception as e:
        return False, [f"Error converting markdown: {str(e)}"]

def convert_html_to_org(input_file: str, output_file: str) -> Tuple[bool, List[str]]:
    """Convert HTML to Org format"""
    try:
        import html2text
        h = html2text.HTML2Text()
        h.body_width = 0  
        
        with open(input_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # 转换为 markdown
        md_content = h.handle(content)
        
        # 然后转换为 org
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(md_content)
        return True, []
    except ImportError:
        return False, ["html2text module not installed"]
    except Exception as e:
        return False, [f"Error converting HTML: {str(e)}"]

def convert_epub_to_org(input_file: str, output_file: str) -> Tuple[bool, List[str]]:
    """
    将 EPUB 转换为 org 格式，使用多种方法尝试转换
    """
    errors = []
    
    # 1. 首先尝试使用 Calibre（如果已安装）
    if EBOOK_CONVERT_PATH:
        try:
            temp_txt = input_file + '.txt'
            result = subprocess.run(
                [EBOOK_CONVERT_PATH, input_file, temp_txt],
                check=True,
                capture_output=True,
                text=True
            )
            
            with open(temp_txt, 'r', encoding='utf-8') as f:
                content = f.read()
            
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write(content)
            
            os.remove(temp_txt)  # 清理临时文件
            return True, []
            
        except subprocess.CalledProcessError as e:
            errors.append(f"Calibre conversion failed: {e.stderr}")
        except Exception as e:
            errors.append(f"Calibre processing error: {str(e)}")
    
    try:
        import ebooklib
        from ebooklib import epub
        from bs4 import BeautifulSoup
        import warnings
        
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            
            book = epub.read_epub(input_file, options={'ignore_ncx': True})
            
            content = []
            
            if book.title:
                content.append(f"#+TITLE: {book.title}\n")
            
            
            metadata = []
            if book.metadata:
                for meta in book.metadata:
                    if meta:
                        metadata.append(f"#+{meta[0].upper()}: {meta[1]}")
            if metadata:
                content.extend(metadata)
                content.append("")  # 添加空行
        
            
            for item in book.get_items():
                if item.get_type() == ebooklib.ITEM_DOCUMENT:
                    try:
                        soup = BeautifulSoup(item.get_content(), 'html.parser')
                        
                        
                        for h in soup.find_all(['h1', 'h2', 'h3', 'h4', 'h5', 'h6']):
                            level = int(h.name[1])
                            h.replace_with(f"{'*' * level} {h.get_text()}\n")
                        
                        
                        for p in soup.find_all('p'):
                            p.replace_with(f"{p.get_text()}\n\n")
                        
                        text = soup.get_text()
                        if text.strip():
                            content.append(text)
                    except Exception as e:
                        errors.append(f"Error processing EPUB item: {str(e)}")
                        continue
            
            
            with open(output_file, 'w', encoding='utf-8') as f:
                f.write('\n'.join(content))
            
            return True, []
            
    except ImportError:
        errors.append("ebooklib or beautifulsoup4 not installed")
    except Exception as e:
        errors.append(f"EPUB processing error: {str(e)}")
    

    if errors:
        logger.error(f"All EPUB conversion methods failed for {input_file}")
        for error in errors:
            logger.error(f"  - {error}")
    
    return False, errors

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
            content = re.sub(r'={3,}', lambda m: '*' * len(m.group()), content)
            content = re.sub(r'-{3,}', lambda m: '*' * len(m.group()), content)
            
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

def process_file(file: str, temp_folder: str, reference_folder: str, archive_folder: str) -> bool:
    """Process a single file"""
    try:
        input_file = os.path.join(temp_folder, file)
        safe_filename = sanitize_filename(os.path.splitext(file)[0])
        output_file = os.path.join(reference_folder, safe_filename + '.org')
        
        success = False
        errors = []
        
        # 检查文件类型
        if file.lower().endswith('.mobi'):
            logger.warning(f"MOBI format is not supported: {file}")
            logger.warning("Please use Calibre to convert MOBI to EPUB first:")
            logger.warning("1. Install Calibre from https://calibre-ebook.com/")
            logger.warning("2. Convert using: ebook-convert input.mobi output.epub")
            logger.warning("3. Then try processing the EPUB file")
            return False
        
        # 处理支持的文件类型
        if file.lower().endswith('.pdf'):
            success, errors = convert_pdf_to_org(input_file, output_file)
        elif file.lower().endswith('.md'):
            success, errors = convert_markdown_to_org(input_file, output_file)
        elif file.lower().endswith('.html'):
            success, errors = convert_html_to_org(input_file, output_file)
        elif file.lower().endswith('.epub'):
            success, errors = convert_epub_to_org(input_file, output_file)
        elif file.lower().endswith(('.txt', '.rst')):
            success, errors = convert_text_to_org(input_file, output_file)
        else:
            logger.warning(f"Unsupported file format: {file}")
            return False
        
        if success:
            logger.info(f"Converted: {input_file} -> {output_file}")
            try:
                archive_path = os.path.join(archive_folder, file)
                shutil.move(input_file, archive_path)
                logger.info(f"Archived: {input_file} -> {archive_path}")
            except Exception as e:
                logger.error(f"Error archiving file: {e}")
            return True
        else:
            logger.error(f"Failed to process {file}: {', '.join(errors)}")
            return False
            
    except Exception as e:
        logger.error(f"Error processing {file}: {str(e)}")
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

def main():
    """Main function"""
    parser = argparse.ArgumentParser(description='Convert documents to org format')
    parser.add_argument('--temp', required=True, help='Temporary folder path')
    parser.add_argument('--reference', required=True, help='Reference folder path')
    parser.add_argument('--archive', required=True, help='Archive folder path')
    
    args = parser.parse_args()
    
    for path in [args.temp, args.reference, args.archive]:
        os.makedirs(path, exist_ok=True)
    

    file_types = ['.md', '.html', '.epub', '.pdf', '.txt', '.rst']  # 移除 .mobi
    unprocessed_files = []
    
    for file_type in file_types:
        logger.info(f"\nProcessing {file_type.upper()} files...")
        files = [f for f in os.listdir(args.temp) if f.lower().endswith(file_type)]
        
        for file in files:
            if not process_file(file, args.temp, args.reference, args.archive):
                unprocessed_files.append(file)

    mobi_files = [f for f in os.listdir(args.temp) if f.lower().endswith('.mobi')]
    if mobi_files:
        logger.warning("\nFound MOBI files that need conversion:")
        for mobi_file in mobi_files:
            logger.warning(f"- {mobi_file}")
        logger.warning("\nPlease convert MOBI files to EPUB using Calibre:")
        logger.warning("1. Install Calibre from https://calibre-ebook.com/")
        logger.warning("2. Convert using: ebook-convert input.mobi output.epub")
        logger.warning("3. Then run this script again with the EPUB files")
    
    if unprocessed_files:
        logger.warning("\nUnprocessed files:")
        for file in unprocessed_files:
            logger.warning(f"- {file}")
    else:
        logger.info("\nAll supported files processed successfully.")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logger.info("\nProcess interrupted by user")
        sys.exit(1)
    except Exception as e:
        logger.error(f"An unexpected error occurred: {e}")
        sys.exit(1)