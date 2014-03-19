Authority management system
===========================

This is a solution to a recruitement task at projectdeltasea
[Description](https://github.com/projectdeltasea/projectdelta/blob/master/README.md),
[Reddit](http://www.reddit.com/r/haskell/comments/1timmt/rebuild_a_million_customer_company_from_php_to/).

There are some groups of people in your domains and some privileges.
There are two many-to-many relationships: People <-> Groups and
Groups <-> Privileges. Of course, you want these relationships to
persist between application runs.

This package will provide basic functionality for querying and CRUDing.

Open questions:
    * How do you let the user configure DB layer?
