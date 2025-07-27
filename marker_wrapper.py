#!/usr/bin/env python3
"""
Marker PDF OCR Wrapper - 独立进程包装器
解决 marker-pdf 的进程泄漏问题
"""
import sys
import json
import signal
import multiprocessing
import warnings
import os
import traceback
from pathlib import Path

# 忽略警告
warnings.filterwarnings("ignore")

# 设置多进程启动方法
if __name__ == "__main__":
    # 在主进程中设置启动方法
    multiprocessing.set_start_method('spawn', force=True)

def signal_handler(signum, frame):
    """处理信号，强制退出"""
    print(f"Received signal {signum}, forcing exit...", file=sys.stderr)
    os._exit(1)

def cleanup_processes():
    """清理所有子进程"""
    try:
        import psutil
        current_process = psutil.Process()
        children = current_process.children(recursive=True)
        
        for child in children:
            try:
                child.terminate()
            except psutil.NoSuchProcess:
                pass
        
        # 等待子进程结束
        import time
        time.sleep(1)
        
        # 强制杀死仍在运行的子进程
        for child in children:
            try:
                if child.is_running():
                    child.kill()
            except psutil.NoSuchProcess:
                pass
                
    except ImportError:
        # 如果没有 psutil，使用基本清理
        import gc
        gc.collect()

def convert_pdf_with_marker(pdf_path: str, result_file: str):
    """使用 Marker 转换 PDF"""
    try:
        import gc
        import torch
        
        from marker.converters.pdf import PdfConverter
        from marker.models import create_model_dict
        from marker.output import text_from_rendered
        
        print(f"🚀 启动 Marker 转换: {pdf_path}")
        
        # 创建转换器和模型  
        print("📦 正在加载 AI 模型...")
        model_dict = create_model_dict()
        print("✓ AI 模型加载完成")
        
        print("🔧 创建 PDF 转换器...")
        converter = PdfConverter(artifact_dict=model_dict)
        print("✓ PDF 转换器创建完成")
        
        try:
            # 转换 PDF
            print("🔄 开始 PDF OCR 转换...")
            rendered = converter(pdf_path)
            print("✓ PDF 渲染完成")
            
            print("📝 提取文本和图片...")
            markdown_content, meta, images = text_from_rendered(rendered)
            print("✓ 文本提取完成")
            
            # 准备结果
            result = {
                "success": True,
                "content": markdown_content,
                "image_count": len(images) if images else 0,
                "error": None,
                "meta": meta if meta else {}
            }
            
            print(f"✅ 转换成功！提取了 {len(images) if images else 0} 个图片，文本长度: {len(markdown_content)} 字符")
            
        finally:
            # 强制清理GPU内存和缓存
            try:
                if torch.cuda.is_available():
                    torch.cuda.empty_cache()
                    print("🧹 GPU 缓存已清理")
                
                # 强制垃圾回收
                gc.collect()
                print("🧹 内存清理完成")
            except Exception as cleanup_e:
                print(f"清理资源时出错: {cleanup_e}")
        
    except Exception as e:
        print(f"Conversion failed: {str(e)}", file=sys.stderr)
        print(traceback.format_exc(), file=sys.stderr)
        
        result = {
            "success": False,
            "content": "",
            "image_count": 0,
            "error": str(e),
            "meta": {}
        }
    
    # 写入结果
    try:
        with open(result_file, "w", encoding="utf-8") as f:
            json.dump(result, f, ensure_ascii=False, indent=2)
        print(f"Results written to: {result_file}", file=sys.stderr)
    except Exception as e:
        print(f"Failed to write results: {e}", file=sys.stderr)
        sys.exit(1)

def main():
    """主函数"""
    if len(sys.argv) != 3:
        print("Usage: marker_wrapper.py <pdf_path> <result_file>", file=sys.stderr)
        sys.exit(1)
    
    pdf_path = sys.argv[1]
    result_file = sys.argv[2]
    
    # 设置信号处理器
    signal.signal(signal.SIGTERM, signal_handler)
    signal.signal(signal.SIGINT, signal_handler)
    
    try:
        # 验证输入文件
        if not Path(pdf_path).exists():
            raise FileNotFoundError(f"PDF file not found: {pdf_path}")
        
        # 执行转换
        convert_pdf_with_marker(pdf_path, result_file)
        
    except Exception as e:
        print(f"Fatal error: {e}", file=sys.stderr)
        
        # 写入错误结果
        error_result = {
            "success": False,
            "content": "",
            "image_count": 0,
            "error": str(e),
            "meta": {}
        }
        
        try:
            with open(result_file, "w", encoding="utf-8") as f:
                json.dump(error_result, f, ensure_ascii=False, indent=2)
        except Exception:
            pass
        
        sys.exit(1)
    
    finally:
        # 清理进程
        cleanup_processes()
        print("Cleanup completed", file=sys.stderr)

if __name__ == "__main__":
    main()