#!/usr/bin/env python3

import fractions
import json
import re
import os

from collections import defaultdict
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from hashlib import sha256, md5
from itertools import groupby
from orgparse import load as org_load
from orgparse.node import OrgNode
from pathlib import Path
from typing import *

file_location = os.path.expanduser("~/Documents/Kindle/My Clippings.txt")
os.environ["PYTHONHASHSEED"] = "157"


# Enums
class AnnotationType(Enum):
    Highlight = "Highlight"
    Note = "Note"
    Bookmark = "Bookmark"


class Todo(Enum):
    Todo = "TODO"
    Strt = "STRT"
    Proj = "PROJ"
    Wait = "WAIT"
    Hold = "HOLD"
    Done = "DONE"
    Kill = "KILL"


# Type definitions
PotentialRange = Tuple[Union[str, int], Optional[Union[str, int]]]
DateTime = datetime
Hash = int
BookTitle = str
AuthorName = str
AnnotationHash = int
BookHashes = Dict[BookTitle, Union[BookTitle, Hash, List[AnnotationHash]]]
AuthorHashes = Dict[AuthorName, Union[BookTitle, Hash, BookHashes]]

# Utility functions
utf8 = lambda o: bytes(str(o), "utf-8")


def nested_set(dic, keys, value):
    for key in keys[:-1]:
        dic = dic.setdefault(key, {})
    dic[keys[-1]] = value


def write_properties(d: Dict[str, Union[int, str]]) -> str:
    props = "\n".join([f":{key.upper()}: {value}" for key, value in d.items() if value])
    return f"\n:PROPERTIES:\n{props}\n:END:\n"


def roman_to_float(s: Optional[Union[str, int]]) -> Optional[float]:
    """If it's actually an int, or None, pop it back out, otherwise turn it to a
float that's 1/100th of its roman value so it sorts at the bottom."""
    if isinstance(s, int):
        return s

    if not s:
        return None

    rom_val = {"I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000}
    int_val = 0
    s = s.upper()
    for i in range(len(s)):
        if i > 0 and rom_val[s[i]] > rom_val[s[i - 1]]:
            int_val += rom_val[s[i]] - 2 * rom_val[s[i - 1]]
        else:
            int_val += rom_val[s[i]]
    return fractions.Fraction(int_val, 10_000)


@dataclass
class Annotation:
    atype: AnnotationType
    title: BookTitle
    page_number: Optional[PotentialRange]
    location: Optional[PotentialRange]
    creation_date: DateTime
    selection: Optional[str]
    my_note: Optional[str]
    status: Optional[Todo]
    body: Optional[str]

    def __lt__(self, other) -> bool:
        """For sorting"""
        rex = r"^the |a "
        self_title = re.sub(rex, "", self.title.lower())
        other_title = re.sub(rex, "", other.title.lower())

        if self_title != other_title:
            return self_title < other_title
        else:
            self_page = roman_to_float(self.page_number[0]) or -1
            other_page = roman_to_float(other.page_number[0]) or -1

            if self_page != other_page:
                return self_page < other_page
            else:
                self_location = self.location[0] or -1
                other_location = other.location[0] or -1
                if self_location != other_location:
                    return self_location < other_location
                else:
                    return self.creation_date < other.creation_date
        return False

    def __hash__(self):
        m = md5()
        for f in [
            self.page_number,
            self.location,
            self.atype.name,
            self.selection,
            self.my_note,
            self.creation_date,
            self.status.name,
            self.body,
        ]:
            m.update(utf8(f))
        return int(m.hexdigest(), 16)

    def to_org(self, depth: int) -> str:
        props = {}
        lp, rp = self.page_number
        if lp or rp:
            if lp and rp:
                props["page"] = f"{lp}-{rp}"
            else:
                props["page"] = f"{lp}"

        ll, rl = self.location
        if ll or rl:
            if ll and rl:
                props["location"] = f"{ll}-{rl}"
            else:
                props["location"] = f"{ll}"

        props["note"] = self.my_note
        props["highlight"] = self.selection
        props["creation_date"] = str(self.creation_date)
        props["id"] = hash(self)
        # checkbox = f"{'[ ]' if self.atype != AnnotationType.Bookmark else ''}"
        return f"""{'*' * depth} {self.status.value} {self.atype.name}{write_properties(props)}{body}"""

    @staticmethod
    def from_org(node: OrgNode) -> Annotation:
        atype: AnnotationType = AnnotationType(node.heading)
        status: Todo = Todo(node.todo)
        author: AuthorName = node.heading
        props = node.properties
        return Annotation(
            atype=atype,
            title=props.get("TITLE"),
            page_number=page_or_location(props.get("PAGE")),
            location=page_or_location(props.get("LOCATION")),
            selection=props.get("HIGHLIGHT"),
            my_note=props.get("NOTE"),
            creation_date=datetime.fromisoformat(props.get("CREATION_DATE")),
            status=Todo(node.todo),
            body=node.body if node.body else None,
        )


@dataclass
class Book:
    title: str
    author: str
    series: str
    annotations: List[Annotation]
    body: Optional[str]

    def __hash__(self):
        m = md5()
        for n in [self.title, self.author, self.series, self.body]:
            m.update(utf8(n))
        for a in self.annotations:
            ah = hash(a)
            m.update(utf8(ah))
        return int(m.hexdigest(), 16)

    def __repr__(self):
        atypes = [
            i.name
            for i in sorted([i.atype for i in self.annotations], key=lambda a: a.value)
        ]
        groups = groupby(atypes)
        friendly = {k: len(list(g)) for k, g in groups}
        return f"Book(title: {self.title}, total: {len(atypes)}, breakdown: {friendly})"

    def to_org(self, depth: int) -> str:
        props = {"author": self.author, "series": self.series, "id": hash(self)}
        s = f"""{'*' * depth} {self.title} [/]{write_properties(props)}"""
        return s + "\n".join([a.to_org(depth + 1) for a in self.annotations])

    @staticmethod
    def from_org(node: OrgNode) -> Book:
        annotations: List[Annotation] = [
            Annotation.from_org(anode) for anode in node.children
        ]
        return Book(
            title=node.heading,
            author=node.get_property("AUTHOR"),
            series=node.get_property("SERIES"),
            annotations=annotations,
            body=node.body if node.body else None,
        )

    def get_hashes(self) -> BookHashes:
        children = [hash(anno) for anno in self.annotations]
        return {"name": self.title, "hash": hash(self), "children": children}


@dataclass
class Author:
    author: str
    books: Dict[BookTitle, Book]
    body: Optional[str]

    def __hash__(self):
        m = md5()
        m.update(utf8(self.author))
        m.update(utf8(self.body))
        for b in self.books:
            bh = hash(b)
            m.update(utf8(bh))
        return int(m.hexdigest(), 16)

    def to_org(self) -> str:
        depth = 1
        props = {"id": hash(self)}
        s = f"""* [ ] {self.author} {write_properties(props)}"""
        return s + "\n".join([b.to_org(depth + 1) for b in self.books])

    @staticmethod
    def from_org(node: OrgNode) -> Author:
        child_books: Dict[BookTitle, Book] = {
            bnode.heading: Book.from_org(bnode) for bnode in node.children
        }
        return Author(
            author=node.heading,
            books=child_books,
            body=node.body if node.body else None,
        )

    def get_hashes(self) -> AuthorHashes:
        children: AuthorHashes = {}
        for book in self.books:
            children[book.title] = book.get_hashes()
        return {
            "hash": hash(self),
            "children": children,
        }


def page_or_location(s: str) -> PotentialRange:
    last = s.split(" ")[-1]
    ls = last.split("-")

    def safe_int(s: str) -> Union[str, int]:
        try:
            return int(s)
        except:
            return s

    range_: PotentialRange = (
        safe_int(ls[0]),
        None if len(ls) == 1 else safe_int(ls[1]),
    )
    return range_


def get_title_author_series(s: str) -> Tuple[str, Optional[str], Optional[str]]:
    # NOTE This is vulnerable to parens in titles
    # Match everything until parentheses, or until end
    author, series = None, None
    title = re.findall(r"(^[^\(]+)", s)[0].strip()
    # Match all things inside parens, returning a list
    regex = r"\(([^\)]+)\)"
    author_series = re.findall(regex, s)

    if len(author_series) == 1:
        author = author_series[0]
    elif len(author_series) == 2:
        # They're in reverse order, series first.
        author, series = author_series[1], author_series[0]

    return title, author, series


def get_date(s: str) -> DateTime:
    return datetime.strptime(s, "Added on %A, %B %d, %Y %I:%M:%S %p")


def handle_metadata(lines: List[str]) -> Annotation:
    md_line = [i.strip() for i in lines[1].split("|")]
    # Long version: - Your Highlight on page 253 | Location 3870-3870 | Added on Sunday, September 16, 2018 10:39:43 PM
    # Short version bookmark: - Your Bookmark on page 216 | Added on Monday, August 13, 2018 8:47:47 PM
    # Short version highlight: - Your Highlight on Location 1430-1432 | Added on Thursday, June 11, 2020 11:34:35 PM
    page_loc: str = md_line[0]
    atype, creation_date, selection, my_note = (
        None,
        None,
        None,
        None,
    )
    page_number, location = (None, None), (None, None)
    title, author, series = get_title_author_series(lines[0])

    if "Bookmark" in page_loc:
        atype = AnnotationType.Bookmark
    elif "Note" in page_loc:
        atype = AnnotationType.Note
    else:
        atype = AnnotationType.Highlight

    if len(lines) > 2:
        remaining = [i for i in lines[2:] if i]

    if atype == AnnotationType.Note:
        my_note = "\n".join(remaining).strip() if remaining else None
    elif atype == AnnotationType.Highlight:
        selection = "\n".join(remaining).strip() if remaining else None

    if len(md_line) == 2:
        creation_date = get_date(md_line[1])
        if "Location" in page_loc:
            location = page_or_location(page_loc)
        elif "page" in page_loc:
            page_number = page_or_location(page_loc)

    elif len(md_line) >= 3:
        loc_str = md_line[1]

        page_number = page_or_location(page_loc)
        location = page_or_location(loc_str)
        creation_date = get_date(md_line[2])

    return Annotation(
        atype=atype,
        title=title,
        author=author,
        series=series,
        page_number=page_number,
        location=location,
        creation_date=creation_date,
        selection=selection,
        my_note=my_note,
    )


def parse(sections: List[str]) -> List[Annotation]:
    results: List[Annotation] = []
    prev = None
    for section in sections:
        collated = False
        if section:
            filtered = [i for i in section.split("\n") if i]
            md = handle_metadata(filtered)
            # print(section)

            # Notes are sometimes (but it seems not always) followed by a
            # highlight that defines how the note was highlighted. I'm not sure
            # why this needed to be this way, but whatever.
            if (
                prev
                and md
                and md.atype == AnnotationType.Highlight
                and prev.location[0] == md.location[1]
                and not prev.location[1]
            ):
                md.my_note = prev.my_note
                md.atype = AnnotationType.Note
                collated = True

            if md and not collated and md.atype == AnnotationType.Note:
                prev = md
            else:
                prev = None
                results.append(md)
    return results


def collect_authors(sections: List[str]) -> Dict[str, Author]:
    r = parse(sections)
    r.sort()

    book_groups = groupby(r, lambda i: i.title)
    books: List[Book] = []
    for key, group in book_groups:
        g = list(group)
        title, author, series = key, g[0].author, g[0].series
        book = Book(title=title, author=author, series=series, annotations=g)
        books.append(book)

    authors: Dict[str, Author] = {}
    for book in books:
        if book.author is None:
            author_name = "Unknown"
        else:
            author_name = book.author
        if author_name in authors:
            authors[author_name].books.append(book)
        else:
            author = Author(author=author_name, books=[book])
            authors[author_name] = author

    return authors


def compare_hashes(old: AuthorHashes, new_: AuthorHashes) -> AuthorHashes:
    results: AuthorHashes = {}
    for new_author_name, new_author in new_.items():
        old_author = old.get(new_author_name)
        if not old_author:
            results[new_author_name] = new_author
            continue

        old_hash, new_hash = old_author.get("hash"), new_author.get("hash")
        if old_hash == new_hash:
            # All sublevels are equal
            continue

        else:
            new_author_books = new_author.get("children")
            for new_book_name, new_book in new_author_books.items():
                old_book = old_author.get(new_book_name)
                if not old_book:
                    results[new_author_name] = {new_book_name: new_book}
                    continue

                old_hash, new_hash = old_book.get("hash"), new_book.get("hash")
                if old_hash == new_hash:
                    continue

                else:
                    new_annotations = set(new_book.get("children", [])) - set(
                        old_book.get("children", [])
                    )
                    nested_set(
                        results, [new_author_name, new_book_name], new_annotations
                    )
    return results


def load_existing_books_file(path: Path) -> Dict[AuthorName, AuthorHashes]:
    root = org_load(path)
    authors = Dict[AuthorName, AuthorHashes]

    for author_node in root:
        author_name: AuthorName = author_node.heading
        books: Dict[BookTitle, Book] = {}

        for book_bullet in author_node.children:
            book_title: BookTitle = book_bullet.heading
            annotations: List[AnnotationHashes] = []


# Notes always come before their highlight
if __name__ == "__main__":
    with open(file_location, mode="r") as f:
        fil = f.read()
        # Something to do with how Kindle saves files (endianess)
        # See https://stackoverflow.com/questions/17912307/u-ufeff-in-python-string/17912811#17912811
        fil = fil.replace("\ufeff", "")
        sections = fil.split("==========")[0:-1]

    authors = collect_authors(sections)
    hashes = {}
    for name, author in authors.items():
        hashes[name] = author.get_hashes()

    # try:
    #     with open("hashes.json", "r") as f:
    #         extant_hashes = json.load(f)
    # except:
    #     extant_hashes = {}

    # with open("hashes.json", "w") as f:
    #     json.dump(hashes, f, indent=4)

    out_str = "\n".join([author.to_org() for author in authors.values()])
    with open("books.org", "w") as f:
        f.write(out_str)
        pass
