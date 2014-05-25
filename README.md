box
===

A query tool for servers.

### original thoughts

This would involve a number of steps (including some things outside of tarski):
 - All instances would be tagged with a unique "id" and a "group"
 - The "group" would be something like we have now <customer>:<role>
 - The "id" would be <group>:<identifier> where identifier is just a random word
   that it easy to spell (could be colour, animals, what ever)
 - So for example,  `group=barney:live`, `id=barney:live:green`.
 - We should then add tooling support for doing things in terms of these tags. All
   workflow/scheduled tasks etc... should be done in terms of tag.

As examples of things I would like to be able to do:

```
ssh $(box barney:live:green)         # ssh into the barney live green box
ssh $(box barney:live)               # ssh into any available barney live box
ssh $(box barney:live|latest)        # ssh into the latest available barney live box
ssh $(box barney:live|copper>1.2.3)  # ssh into any live box that has copper > 1.2.3 on it
ssh $(box barney:live|active)        # ssh into any barney live box that has an "active" tag on it.
```

This would allow us to specify cron and workflows in terms of these
mechanisms.


Other pre-req for full solution is bakery and tagging instances with
software versions (via aws or other means), but a number of steps can
be taken in before that.


