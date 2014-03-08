Authority management system
===========================

There are some groups of people in your domains and some privileges.
There are two many-to-many relationships: People <-> Groups and
Groups <-> Privileges. Of course, you want these relationships to
persist between application runs.

This package will provide basic functionality for querying and CRUDing.

Open questions:
    * How do you let the user configure DB layer?
