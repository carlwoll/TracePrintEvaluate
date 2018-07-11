# TracePrintEvaluate

TracePrintEvaluate prints evaluations matching a pattern, along with additional information

Each trace print output can be clicked on to switch to viewing a different piece of information. Right clicking the output copies the information.

Here's an example of the output:

```
TracePrintEvaluate[f[4], _f]

 f[4]

  f[4-1]

  f[3]

   f[3-1]

   f[2]

   f[3-2]

   f[1]

  f[4-2]

  f[2]

3
```

And here is an example of the output after clicking on the first three outputs:

```
TracePrintEvaluate[f[4], _f]

 3

 2

 2

   f[3-1]

   f[2]

   f[3-2]

   f[1]

  f[4-2]

  f[2]

 3
 ```
