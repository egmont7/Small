#!/usr/bin/env python3
import requests
import argparse
import bs4

def main(args):
    url = args.url
    out_file_name = args.outputFile
    
    r = requests.get(url)
    soup = bs4.BeautifulSoup(r.text, "lxml")
    EN = soup.findAll(class_ = 'TermText qDef lang-en')
    DE = soup.findAll(class_ = 'TermText qWord lang-de')
    if len(EN) == 0: #Try reverse
        EN = soup.findAll(class_ = 'TermText qWord lang-en')
        DE = soup.findAll(class_ = 'TermText qDef lang-de')
    pairs = [(d.text, e.text) for d, e in zip(DE,EN)]
    with open(out_file_name,'w') as f:
        for en, de in pairs:
            f.write("{}   ::   {}\n".format(en.replace('\n',', '), de.replace('\n',', ')))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("url", help="The URL of the source words")
    parser.add_argument("outputFile", help="File to save to")
    ARGS = parser.parse_args()

    main(ARGS)

