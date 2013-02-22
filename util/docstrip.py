#!/usr/bin/python2
import os
import sys
import codecs
import shutil
import bs4

def isParameterTag(i):
	if not isinstance(i, bs4.element.Tag):
		return False
	if i.name != "code":
		return False
	if 'class' not in i.attrs or "pparameter" not in i['class']:
		return False
	return True

def isExamplesTag(i):
	if not isinstance(i, bs4.element.Tag):
		return False
	if i.name != "b":
		return False
	if 'class' not in i.attrs or "pnote" not in i['class']:
		return False
	if i.string is None or (u"Example" not in i.string and u"example" not in i.string):
		return False
	return True

def isSyntaxNoteTag(i):
	if not isinstance(i, bs4.element.Tag):
		return False
	if i.name != "p":
		return False
	if 'class' not in i.attrs or "pBody" not in i['class']:
		return False
	if i.string is None or "do not represent syntax" not in i.string:
		return False
	return True

def wrapped_var_slots(lst):
	for t in lst:
		if isinstance(t, bs4.element.Tag) and t.name == 'code':
			s = t.string.strip()
			if len(s) > 0:
				yield u"`{}`".format(s)
		else:
			yield t

def filter_pre(pre):
	tags = (t for t in pre.contents if not isinstance(t, bs4.element.Tag) or t.name == 'code')
	return wrapped_var_slots(tags)
	
def syntax_section(soup):
	h4s = soup.find_all('h4')
	if len(h4s) == 0:
		return (None, False)
	syntax = h4s[0]
	if syntax.string != u'\r\nSyntax\r\n':
		return (None, False)

	# Go find the syntax sections
	special = False
	syntax_tables = []
	for i in syntax.find_next_siblings():
		special = False
		if not isinstance(i, bs4.element.Tag):
			return (syntax_tables, special)
		if isSyntaxNoteTag(i):
			special = True
			syntax_tables.append(i)
		if i.name == "div" and i.pre is not None:
			i.pre.string = ''.join(filter_pre(i.pre))
			syntax_tables.append(i)
	return (syntax_tables, special)

def syntax_html(soup):
	outsoup = bs4.BeautifulSoup("<ul></ul")
	tag = soup.ul
	stuffs = syntax_section(soup)
	if stuffs is None:
		new_tag = outsoup.new_tag("li")
		msg = outsoup.new_tag("p")
		msg.string = "No syntax section found in file"
		new_tag.append(msg)
		tag.append(new_tag)
		return tag
	for t in stuffs[0]:
		new_tag = outsoup.new_tag("li")
		new_tag.append(t)
		tag.append(new_tag)
	return tag

def syntax_strings(soup):
	sections, special = syntax_section(soup)
	if sections is None:
		return []
	return [t.pre.string for t in sections if not isSyntaxNoteTag(t)]

def syntax_page(filename):
	fh = open(filename)
	soup = bs4.BeautifulSoup("<html><head><title>Syntax Page</title></head><body></body></html")
	t = syntax_html(fh)
	soup.body.append(t)
	print soup
	fh.close()

def file_title(soup):
	return soup.h2.string.strip()

def html_files(d):
	for root, dirs, files in os.walk(d):
		for f in files:
			fname, fext = os.path.splitext(f)
			if fext == ".html":
				print "processing {}".format(os.path.join(root, f))
				yield os.path.join(root, f)


def make_html_syntax_page(d):
	soup = bs4.BeautifulSoup('<html><head><META http-equiv="Content-Type" content="text/html; charset=iso-8859-1"><title>Syntax Page</title></head><body</body></html>')

	fs = html_files(d)
	for f in fs:
		fh = open(f)
		t = syntax_html(fh)
		header = soup.new_tag("h4")
		header.string = os.path.split(f)[1]
		new_t = soup.new_tag("div")
		new_t.append(t)
		soup.body.append(header)
		soup.body.append(new_t)
		fh.close()
	print soup

def ensure_directory(d):
	if not os.path.exists(d):
		os.makedirs(d)

def make_syntax_tree(src, dest):
	# Clean dest first
	shutil.rmtree(dest)
	os.makedirs(dest)

	fs = html_files(src)
	for f in fs:
		with open(f) as fh:
			fname, fext = os.path.splitext(os.path.split(f)[1])
			destdir = os.path.join(dest, fname)

			soup = bs4.BeautifulSoup(fh)
			strings = syntax_strings(soup)
			if len(strings) == 0:
				continue
			fh.seek(0)
			ensure_directory(destdir)
			with open(os.path.join(destdir, "title"), 'w') as fname_file:
				fname_file.write(file_title(soup))
			for i in range(len(strings)):
				out = codecs.open(os.path.join(destdir, '{}.txt'.format(i)), 'w', "utf-8")
				out.write(strings[i])
				out.close()
		

if __name__ == "__main__":
	make_syntax_tree(sys.argv[1], sys.argv[2])