Based on https://github.com/todotxt/todo.txt

Built in Delphi 11.3 (Community Edition)
Version 0.1

COMMANDS IMPLEMENTED (SO FAR)

todo -i "an item to add to the todo list"
todo -l
todo -la
todo -d:2,3,4
todo -t

todo -add "an item to add to the todo list"
todo -list
todo -listall
todo -delete:2,5,1
todo -touch


2024-7-27 - WHAT ARE WE DOING TODAY

Delete
- parmeters are -d or -del and IDs to delete
- removes items from list
- see notes below on deleting from a string list

Touch
- parmeters are -t or -touch
- check all item have a started date
- only process items not completed actually
- if not, sets started date to now

Archive
- parmeters are -a or -archive
- moves completed items to an archive
- these are rows marked with an 'x'

On deleting items from a stringlist -

-------------------------------------------------------------------------------
0 1 2 3 4 5 6 7
-------------------------------------------------------------------------------
intial list
A B C D E F G H

afer deleting items[0]     1,7
A B C D E F G

items[7] not longer exists!
-------------------------------------------------------------------------------

- on deleting items from stringlist have to go in reverse order
- why? if you delete 1st item, then the 2nd becomes the first, 3rd becomes second etc.
- if there are 10 items in the list (0..9) and you wanted to delete the 1st and last then
- after deleting the 1st, the last item is now in position 8
- at least when done in reverse order you can preserve order of items not processed
- if you did want to process in other then you would need some sort of deleted flag
- and if on saving might want to have some sort of compacting type function to remove those
- items marked for deletion.
