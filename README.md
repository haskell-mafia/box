box
===

A query tool for servers.

### query type thoughts

We are exposed to what is published by power (this can be changed), but for now is:
 - Instance ID
 - Private IP
 - Public IP
 - Machine name (encodes other info as well)
 - Client
 - Flavour

As examples of things I would like to be able to do:

Where `barney` is an example customer.

```
box ssh barney:live:kermit.green  # ssh into the barney live  box named kermit.green box
box ssh barney:live               # ssh into any available barney live box
```





