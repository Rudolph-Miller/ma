#!/usr/bin/env python
# coding: UTF-8
import urllib, urllib2, sys

OPENER = urllib2.build_opener()
OPENER.addheaders = [("User-Agent", "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")]
base = "https://www.google.co.jp/search?hl=ja&num=1&q="
base2 = "http://search.yahoo.co.jp/search?p="
base3 = "http://search.livedoor.com/search?q="

def get_result (que) :
				url = base +  urllib.quote("\"%s\"" % que)
				html = OPENER.open(url).read()
				html = html.decode("utf-8", "ignore").encode("utf-8") 
				print html

def yahoo (que) :
				url = base2 +  urllib.quote("\"%s\"" % que)
				html = OPENER.open(url).read()
				html = html.decode("utf-8", "ignore").encode("utf-8") 
				print html

def livedoor (que) :
				url = base3 +  urllib.quote("\"%s\"" % que)
				html = OPENER.open(url).read()
				html = html.decode("utf-8", "ignore").encode("utf-8") 
				print html

if __name__ == "__main__":
				if sys.argv[2] == "yahoo":
								yahoo (sys.argv[1])
				elif sys.argv[2] == "livedoor":
								livedoor (sys.argv[1])
				else:
								get_result (sys.argv[1])
sys.exit(0)
