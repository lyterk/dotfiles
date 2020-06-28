#!/usr/bin/env ipython

from transform_clippings import *

test_sections = """The Lord of the Rings (J. R. R. Tolkien)
- Your Note on Location 16415 | Added on Wednesday, September 25, 2019 10:59:56 PM

Translate
==========
The Lord of the Rings (J. R. R. Tolkien)
- Your Highlight on Location 16415-16415 | Added on Wednesday, September 25, 2019 10:59:56 PM

dwimmerlaik,
==========
The Lion, The Witch, and the Wardrobe
- Your Note on Location 15247 | Added on Sunday, October 6, 2019 8:19:20 AM

Heh
==========
The Lion, The Witch, and the Wardrobe
- Your Highlight on Location 15245-15247 | Added on Sunday, October 6, 2019 8:19:20 AM

Edmund did something bad
==========
Verses (Christ, Jesus)
- Your Highlight on page 331-335 | Location 5365-5366 | Added on Tuesday, June 16, 2020 1:14:43 AM

Jesus wept
=========="""


def test_roman_to_float():
    t = "XVII"
    u = "cc"
    v = "mmxx"

    assert roman_to_float(t) == fractions.Fraction(17, 10_000)
    assert roman_to_float(u) == fractions.Fraction(1, 50)
    assert roman_to_float(v) == fractions.Fraction(101, 500)


def test_get_title_author_series():
    tas = "Prince Caspian (Chronicles of Narnia) (C.S. Lewis)"
    ta = "Prince Caspian (C.S. Lewis)"
    t = "Prince Caspian "

    (title, author, series) = get_title_author_series(tas)
    assert title == "Prince Caspian"
    assert "C.S. Lewis" == author
    assert "Chronicles of Narnia" == series

    (title, author, series) = get_title_author_series(ta)
    assert "Prince Caspian" == title
    assert "C.S. Lewis" == author

    (title, author, series) = get_title_author_series(t)
    assert "Prince Caspian" == title


def test_get_date():
    s = "Added on Sunday, August 5, 2018 4:24:07 PM"
    dt = get_date(s)
    assert dt.day == 5
    assert dt.year == 2018
    assert dt.hour == 16


def test_page_or_location():
    a = "- Your Highlight on page 1"
    b = "Location 1-2"
    c = "- Your Highlight on Location 1-2"

    ax = page_or_location(a)
    assert ax[0] == 1 and ax[1] == None
    bx = page_or_location(b)
    assert bx[0] == 1 and bx[1] == 2
    cx = page_or_location(c)
    assert cx[0] == 1 and cx[1] == 2


def test_handle_metadata():
    a = """The City We Became (The Great Cities Trilogy) (Jemisin, N. K.)
- Your Highlight on page 289 | Location 3893-3893 | Added on Friday, June 5, 2020 8:20:03 PM

the only true justice is having the strength to protect oneself against invasion or conquest.


    """

    b = """The Robots of Dawn (Isaac Asimov)
- Your Bookmark on page 201 | Location 3080 | Added on Saturday, August 4, 2018 9:11:12 PM
    """

    ax = handle_metadata(a.split("\n"))
    assert ax.atype == AnnotationType.Highlight
    assert ax.page_number == (289, None)
    assert ax.location == (3893, 3893)
    assert ax.title == "The City We Became"
    assert ax.author == "Jemisin, N. K."
    assert ax.series == "The Great Cities Trilogy"
    assert (
        ax.selection
        == "the only true justice is having the strength to protect oneself against invasion or conquest."
    )

    bx = handle_metadata(b.split("\n"))
    assert bx.atype == AnnotationType.Bookmark
    assert bx.page_number == (201, None)
    assert bx.location == (3080, None)
    assert bx.creation_date.day == 4


def test_parse():
    sections: List[str] = test_sections.split("==========")[0:-1]
    r: List[Annotation] = parse(sections)
    # I wish there was some kind of way to correctly do this destructuring assignment, doesn't seem like it though.
    first, second, third = r[0], r[1], r[2]
    assert first.atype == AnnotationType.Note
    assert first.selection == "dwimmerlaik,"
    assert second.atype == AnnotationType.Note
    assert "Edmund" in second.selection
    assert third.atype == AnnotationType.Highlight
    assert third.location == (5365, 5366)
    assert third.page_number == (331, 335)


def test_lt():
    l = [
        Annotation(
            atype=AnnotationType.Highlight,
            title="Shogun",
            author="James Clavell",
            series=None,
            page_number=(121, None),
            location=(1842, 1842),
            creation_date=datetime(2020, 5, 10, 13, 8, 22),
            selection="gloaming",
            my_note=None,
        ),
        Annotation(
            atype=AnnotationType.Bookmark,
            title="The Stone Sky",
            author="Jemisin, N. K.",
            series="The Broken Earth",
            page_number=(None, None),
            location=(5141, None),
            creation_date=datetime(2019, 3, 15, 18, 16, 20),
            selection=None,
            my_note=None,
        ),
        Annotation(
            atype=AnnotationType.Bookmark,
            title="The Stone Sky",
            author="Jemisin, N. K.",
            series="The Broken Earth",
            page_number=(None, None),
            location=(2741, None),
            creation_date=datetime(2019, 3, 15, 18, 15, 44),
            selection=None,
            my_note=None,
        ),
        Annotation(
            atype=AnnotationType.Bookmark,
            title="The Stone Sky",
            author="Jemisin, N. K.",
            series="The Broken Earth",
            page_number=(None, None),
            location=(2681, None),
            creation_date=datetime(2018, 12, 11, 17, 43, 54),
            selection=None,
            my_note=None,
        ),
    ]
    l.sort()
    assert l[0].title == "Shogun"
    assert l[1].location[0] == 2681
    assert l[3].location[0] == 5141


def test_hashes():
    os.environ["PYTHONHASHSEED"] = "157"
    a = (
        Annotation(
            atype=AnnotationType.Bookmark,
            title="The Stone Sky",
            author="Jemisin, N. K.",
            series="The Broken Earth",
            page_number=(None, None),
            location=(2681, None),
            creation_date=datetime(2018, 12, 11, 17, 43, 54),
            selection=None,
            my_note=None,
        ),
    )
    b = hash(a)
    assert b == 3188988130352475666
    sections = test_sections.split("==========")[0:-1]

    authors = collect_authors(sections)
    book = authors["Unknown"].books[0]
    assert hash(book) == 658861006438507728
    hashes = {
        "Christ, Jesus": 2079789526716109365,
        "J. R. R. Tolkien": 1468033387753723113,
        "Unknown": 478866803134482748,
    }
    for author in authors.values():
        assert hash(author) == hashes[author.author]

    results = {author.author: author.get_hashes() for author in authors.values()}
    assert results["Christ, Jesus"]["children"]["Verses"]["children"] == [
        94464795148025987
    ]
    assert len(results) == 3


def test_compare_hashes():
    sections = test_sections.split("==========")[0:-1]

    authors = collect_authors(sections)
    old_hashes = {author.author: author.get_hashes() for author in authors.values()}

    more_sections = (
        test_sections
        + """The Lord of the Rings (J. R. R. Tolkien)
- Your Note on Location 16417 | Added on Wednesday, September 25, 2019 10:59:56 PM

Translate
==========
The Lord of the Rings (J. R. R. Tolkien)
- Your Highlight on Location 16415-16415 | Added on Wednesday, September 25, 2019 10:59:56 PM

elf,
==========
The Lion, The Witch, and the Wardrobe
- Your Note on Location 15247 | Added on Sunday, October 6, 2019 8:19:20 AM

Heh
==========
The Lion, The Witch, and the Wardrobe
- Your Highlight on Location 15245-15247 | Added on Sunday, October 6, 2019 8:19:20 AM

Edmund did something bad
==========
Gospels (Christ, Jesus)
- Your Highlight on page 331-335 | Location 5365-5366 | Added on Tuesday, June 16, 2020 1:14:43 AM

Jesus wept more
==========
The Amber Spyglass (Pullman, Phillip)
- Your Highlight on Location 15245-15247 | Added on Sunday, October 6, 2019 8:19:20 AM

Lyra no
=========="""
    )

    sections = more_sections.split("==========")[0:-1]

    authors = collect_authors(sections)
    new_hashes = {author.author: author.get_hashes() for author in authors.values()}

    result = compare_hashes(old_hashes, new_hashes)
    answer = {
        "Pullman, Phillip": {
            "name": "Pullman, Phillip",
            "hash": 1174642329411167460,
            "children": {
                "The Amber Spyglass": {
                    "name": "The Amber Spyglass",
                    "hash": 633193797674823442,
                    "children": [1427366540883682735],
                }
            },
        },
        "Christ, Jesus": {
            "Verses": {
                "name": "Verses",
                "hash": 1198047322231062525,
                "children": [94464795148025987],
            }
        },
        "Unknown": {
            "The Lion, The Witch, and the Wardrobe": {
                "name": "The Lion, The Witch, and the Wardrobe",
                "hash": 1560429875865481869,
                "children": [1778811855724295127, 1778811855724295127],
            }
        },
        "J. R. R. Tolkien": {
            "The Lord of the Rings": {
                "name": "The Lord of the Rings",
                "hash": 2267819111823973597,
                "children": [2020902774068906401, 2120035198696789423],
            }
        },
    }

    def test_to_from_org():
        sections = more_sections.split("==========")[0:-1]

        authors = collect_authors(sections)
        new_hashes = {author.author: author.get_hashes() for author in authors.values()}
