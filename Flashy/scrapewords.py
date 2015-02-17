#!/usr/bin/env python3
import requests
import argparse
import bs4

def main(args):
    url = args.url
    out_file_name = args.outputFile
    
    r = requests.get(url)
    soup = bs4.BeautifulSoup(r.text)
    EN = soup.findAll(class_ = 'qDef lang-en')
    DE = soup.findAll(class_ = 'qWord lang-de')
    pairs = [(d.text, e.text) for d, e in zip(DE,EN)]
    with open(out_file_name,'w') as f:
        for en, de in pairs:
            f.write("{}   ::   {}\n".format(en, de))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("url", help="The URL of the source words")
    parser.add_argument("outputFile", help="File to save to")
    ARGS = parser.parse_args()

    main(ARGS)

