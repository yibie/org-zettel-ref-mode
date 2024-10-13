import re
import argparse
import os
import sys

def is_chinese(text):
    return bool(re.search(r'[\u4e00-\u9fff]', text))

def fix_paragraphs(text, add_blank_line=True):
    is_chinese_text = is_chinese(text)
    lines = text.split('\n')
    paragraphs = []
    current_paragraph = ""
    
    for line in lines:
        line = line.strip()
        if not line:
            if current_paragraph:
                paragraphs.append(current_paragraph)
                current_paragraph = ""
            continue
        
        if is_chinese_text:
            if current_paragraph and not re.match(r'^[。！？』」；：]', line):
                current_paragraph += line
            else:
                if current_paragraph:
                    paragraphs.append(current_paragraph)
                current_paragraph = line
        else:
            if current_paragraph and (re.match(r'^[a-z,;:]', line) or not re.match(r'^[A-Z]', line)):
                current_paragraph += " " + line
            else:
                if current_paragraph:
                    paragraphs.append(current_paragraph)
                current_paragraph = line
    
    if current_paragraph:
        paragraphs.append(current_paragraph)
    
    # 根据是否添加空行来选择分隔符
    if add_blank_line:
        separator = "\n\n"
    else:
        separator = "\n" if is_chinese_text else "\n\n"
    
    return separator.join(paragraphs)

def process_file(input_file, overwrite=False, add_blank_line=True):
    try:
        with open(input_file, 'r', encoding='utf-8') as file:
            text = file.read()
        
        fixed_text = fix_paragraphs(text, add_blank_line)
        
        if overwrite:
            with open(input_file, 'w', encoding='utf-8') as file:
                file.write(fixed_text)
            print(f"Processed and overwritten: {input_file}")
        else:
            print(f"Preview of fixed text for {input_file}:")
            print(fixed_text[:500] + "..." if len(fixed_text) > 500 else fixed_text)
            print("\nUse --overwrite to apply changes.")
    except Exception as e:
        print(f"Error processing {input_file}: {str(e)}")

def main():
    parser = argparse.ArgumentParser(description="Fix paragraph formatting in OCR-processed text files.")
    parser.add_argument('files', nargs='+', help='Input text file(s) to process')
    parser.add_argument('--overwrite', action='store_true', help='Overwrite the original files')
    parser.add_argument('--no-blank-line', action='store_true', help='Do not add blank lines between paragraphs')
    
    args = parser.parse_args()
    
    if args.overwrite:
        confirm = input("Are you sure you want to overwrite the original files? This action cannot be undone. (yes/no): ")
        if confirm.lower() != 'yes':
            print("Operation cancelled.")
            sys.exit(0)
    
    for file in args.files:
        process_file(file, args.overwrite, not args.no_blank_line)

if __name__ == "__main__":
    main()
