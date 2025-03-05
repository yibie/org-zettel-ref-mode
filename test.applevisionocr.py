#!/usr/bin/env python3
# applevisionocr.py

import os
import ctypes
import logging
import argparse
from pathlib import Path
from Foundation import NSArray, NSString, NSAutoreleasePool
from AppKit import NSBitmapImageRep, NSImage
from Vision import VNImageRequestHandler, VNRecognizeTextRequest
import Quartz
from pdf2image import convert_from_path
import fitz  # PyMuPDF
from PIL import Image
import io

# 定义常量
kCGRenderingIntentDefault = 0
kCGRenderingIntentAbsoluteColorimetric = 1
kCGRenderingIntentRelativeColorimetric = 2
kCGRenderingIntentPerceptual = 3
kCGRenderingIntentSaturation = 4

# 定义 ctypes 接口
lib = ctypes.CDLL('/System/Library/Frameworks/Quartz.framework/Quartz')

lib.CGDataProviderCreateWithFilename.argtypes = [ctypes.c_char_p]
lib.CGDataProviderCreateWithFilename.restype = ctypes.c_void_p

lib.CGImageCreateWithPNGDataProvider.argtypes = [ctypes.c_void_p, ctypes.POINTER(ctypes.c_float), ctypes.c_bool, ctypes.c_int]
lib.CGImageCreateWithPNGDataProvider.restype = ctypes.c_void_p

lib.CGImageRelease.argtypes = [ctypes.c_void_p]
lib.CGImageRelease.restype = None

lib.CGDataProviderRelease.argtypes = [ctypes.c_void_p]
lib.CGDataProviderRelease.restype = None

# 定义固定的临时目录
TEMP_DIR = Path.home() / "Documents" / "ocr_temp"

def ensure_temp_dir():
    """
    确保临时目录存在，如果不存在则创建一个新的。
    """
    try:
        TEMP_DIR.mkdir(parents=True, exist_ok=True)
        if not os.access(TEMP_DIR, os.W_OK):
            raise PermissionError(f"没有写入权限：{TEMP_DIR}")
        logging.debug(f"使用临时目录：{TEMP_DIR}")
    except Exception as e:
        logging.error(f"无法创建或访问临时目录 {TEMP_DIR}: {e}")
        raise

def preprocess_image(image_path):
    """
    对图像进行预处理，例如转换为灰度图和二值化。
    """
    try:
        with Image.open(image_path) as img:
            img = img.convert('L')  # 转换为灰度图
            img = img.point(lambda x: 0 if x < 128 else 255, '1')  # 二值化
            return img
    except Exception as e:
        logging.error(f"预处理图像 {image_path} 时出错：{e}")
        raise

def ocr_detect_image(cgimage) -> str:
    """
    使用 Vision 框架对 CGImage 进行 OCR 识别。
    """
    try:
        handler = VNImageRequestHandler.alloc().initWithCGImage_options_(cgimage, None)
        request = VNRecognizeTextRequest.alloc().init()
        request.setRecognitionLanguages_(["zh-Hans", "zh-Hant"])  # 简体中文和繁体中文
        request.setUsesLanguageCorrection_(True)
        
        success = handler.performRequests_error_([request], None)
        
        if not success:
            raise Exception("文字识别失败")
        
        results = request.results()
        if results:
            recognized_text = "\n".join([result.topCandidates_(1)[0].string() for result in results])
            return recognized_text
        else:
            return ""
    except Exception as e:
        logging.error(f"OCR 识别出错: {e}")
        return ""

def ocr_detect_png(path: Path) -> str:
    """
    对 PNG 图像进行 OCR 识别。
    """
    try:
        # 预处理图像
        preprocessed_img = preprocess_image(path)
        
        # 将预处理后的图像保存为临时文件
        temp_path = TEMP_DIR / f"preprocessed_{path.name}"
        preprocessed_img.save(temp_path)
        
        # 使用预处理后的图像创建 NSImage
        ns_image = NSImage.alloc().initWithContentsOfFile_(str(temp_path))
        if ns_image is None:
            raise Exception("无法加载图像")
        
        bitmap_rep = NSBitmapImageRep.imageRepWithData_(ns_image.TIFFRepresentation())
        if bitmap_rep is None:
            raise Exception("无法创建位图表示")
        
        cg_image = bitmap_rep.CGImage()
        if cg_image is None:
            raise Exception("无法创建 CGImage")
        
        text = ocr_detect_image(cg_image)
        
        # 删除预处理后的临时文件
        temp_path.unlink()
        
        return text
    except Exception as e:
        logging.error(f"处理图片 {path} 时出错：{e}")
        return ""

def convert_pdf_to_png(pdf_path: Path, dpi: int = 300) -> list:
    """
    使用 pdf2image 或 PyMuPDF 将 PDF 页面转换为 PNG 图像。
    """
    image_paths = []
    try:
        # 首先尝试使用 pdf2image
        logging.debug("尝试使用 pdf2image 转换 PDF 为 PNG")
        images = convert_from_path(pdf_path, dpi=dpi, output_folder=str(TEMP_DIR))
        for i, img in enumerate(images):
            png_path = TEMP_DIR / f"page_{i+1}.png"
            img.save(png_path, "PNG")
            image_paths.append(png_path)
            logging.debug(f"保存了页面 {i+1} 到 {png_path}")
    except Exception as e:
        logging.error(f"使用 pdf2image 转换 PDF 时出错：{e}")
        logging.info("尝试使用 PyMuPDF 转换 PDF 为 PNG")
        try:
            doc = fitz.open(pdf_path)
            for page_num in range(len(doc)):
                page = doc.load_page(page_num)
                pix = page.get_pixmap(matrix=fitz.Matrix(dpi/72, dpi/72))
                image_path = TEMP_DIR / f"page_{page_num+1}.png"
                pix.save(image_path)
                image_paths.append(image_path)
                logging.debug(f"保存了页面 {page_num+1} 到 {image_path}")
            doc.close()
        except Exception as e:
            logging.error(f"使用 PyMuPDF 转换 PDF 时出错：{e}")
    return image_paths

def ocr_detect_pdf(pdf_path: Path, save_txt_path: Path = None, dpi: int = 300) -> str:
    """
    对 PDF 文件进行 OCR 识别，并将结果保存为 TXT 文件。
    """
    logging.info(f"开始 OCR 识别 PDF 文件：{pdf_path}")
    image_paths = convert_pdf_to_png(pdf_path, dpi)
    if not image_paths:
        logging.error("无法提取 PDF 页面")
        return ""
    
    all_text = ""
    for i, image_path in enumerate(image_paths):
        logging.info(f"正在处理第 {i+1} 页，共 {len(image_paths)} 页")
        if not image_path.exists():
            logging.error(f"图片文件不存在：{image_path}")
            continue
        try:
            text = ocr_detect_png(image_path)
            all_text += text + "\n"
        except Exception as e:
            logging.error(f"处理图片 {image_path} 时出错：{e}")
    
    if save_txt_path:
        try:
            save_txt_path.parent.mkdir(parents=True, exist_ok=True)
            with open(save_txt_path, 'w', encoding='utf-8') as f:
                f.write(all_text)
            logging.info(f"OCR 识别完成，结果已保存到 {save_txt_path}")
        except Exception as e:
            logging.error(f"保存 OCR 结果到 {save_txt_path} 时出错：{e}")
    
    return all_text

def main():
    """
    脚本的主函数，解析参数并启动 OCR 过程。
    """
    parser = argparse.ArgumentParser(description="使用 OCR 识别 PDF 文件")
    parser.add_argument('-i', '--input', type=str, required=True, help="输入的 PDF 文件路径")
    parser.add_argument('-o', '--output', type=str, required=True, help="输出的 TXT 文件路径")
    parser.add_argument('-d', '--dpi', type=int, default=300, help="转换图片的分辨率，默认 300 DPI")
    parser.add_argument('-v', '--verbose', action='store_true', help="启用详细日志")
    
    args = parser.parse_args()
    
    # 展开用户路径
    input_path = Path(os.path.expanduser(args.input))
    output_path = Path(os.path.expanduser(args.output))
    
    # 配置日志
    if args.verbose:
        logging.basicConfig(level=logging.DEBUG, format='%(levelname)s: %(message)s')
    else:
        logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    
    # 确保临时目录存在
    try:
        ensure_temp_dir()
    except Exception as e:
        logging.error(f"无法准备临时目录：{e}")
        return
    
    # 开始 OCR 过程
    try:
        recognized_text = ocr_detect_pdf(input_path, output_path, args.dpi)
        if recognized_text:
            logging.info(f"OCR 识别完成，结果已保存到 {output_path}")
        else:
            logging.info("OCR 识别完成，但未检测到任何文本。")
    except Exception as e:
        logging.error(f"OCR 识别失败: {e}")

if __name__ == "__main__":
    main()