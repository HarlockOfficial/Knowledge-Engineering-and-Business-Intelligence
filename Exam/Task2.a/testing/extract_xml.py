from xml.etree import ElementTree as ET


def extract_xml(file_path) -> ET.Element:
    tree_root = ET.parse(file_path).getroot()
    return tree_root
