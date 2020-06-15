#!/usr/bin/env python3

import os
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from typing import *

file_location = os.path.expanduser("~/Documents/Kindle/My Clippings.txt")

PotentialRange = Tuple[int, Optional[int]]
DateTime = datetime


@dataclass
class Annotation:
    page_number: Optional[PotentialRange]
    location: Optional[PotentialRange]
    creation_date: DateTime
    selection: Optional[str]
    my_note: Optional[str]


class AnnotationType(Enum):
    Highlight = 1
    Note = 2
    Bookmark = 3


def handle_metadata(
    ls: List[str],
) -> Tuple[AnnotationType, int, Tuple[int, Optional[int]], datetime]:
    # Long version: - Your Highlight on page 253 | Location 3870-3870 | Added on Sunday, September 16, 2018 10:39:43 PM
    # Short version: - Your Bookmark on page 216 | Added on Monday, August 13, 2018 8:47:47 PM
    your = ls[0]
    if len(ls) == 2:

    if "Note" in your:
        atype = AnnotationType.Note
        loc = ls[1]
    elif "Highlight" in your:
        atype = AnnotationType.Highlight
        loc = ls[1]
    else:
        atype = AnnotationType.Bookmark
        loc = None

    atype = AnnotationType.Note if "Note" in your else AnnotationType.Highlight
    page_strs = your.split(" ")[-1].split("-")
    page_no = (
        int(page_strs[0]),
        None if len(page_strs) == 1 else int(page_strs[1]),
    )

    if loc:
        loc_strs = loc.split(" ")[1].split("-")
        locs = (int(loc_strs[0]), None if len(loc_strs) == 1 else int(loc_strs[1]))
    else:
        locs = None

    dt = datetime.strptime(ls[2], "Added on %A, %B %d, %Y %I:%M:%S %p")
    return (atype, page_no, locs, dt)


with open(file_location, "r") as f:
    fil = f.read()
    sections = fil.split("==========")

# Notes always come before their highlight
prev = None
for section in sections:
    section = [i for i in section.split("\n") if i]
    try:
        title_str = section[0]
        metadata_str = section[1].split(" | ")
        if len(section) > 2:
            note = section[3:]
        else:
            note = None
        md = handle_metadata(metadata_str)
        if md[0] == AnnotationType.Note:
            prev = (md, note)
        else:
            prev = None
        # print(title_str, note)
    except Exception as e:
        print(str(e) + str(section))
