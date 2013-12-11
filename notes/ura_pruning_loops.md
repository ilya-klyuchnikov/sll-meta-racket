## Pruning of idle loops

Consider the following program:

```
[(f (Z) x) = x]
[(f (S n) x) = (f n (F))]
```

and URA task with `in = (f n (T))` and `out = (T)`.

You may look into corresponding [SPSC example](http://spsc.appspot.com/view?key=agRzcHNjcjQLEgZBdXRob3IiGmlseWEua2x5dWNobmlrb3ZAZ21haWwuY29tDAsSB1Byb2dyYW0YgTIM).

```
                [c0] –––––––––
                     (f n (T))
                     –––––––––
                       / \
          n = (S n.1) /   \ n = (Z)
                     /     \
          [c1]–––––––––––   –––[c2]
              (f n.1 (F))   (T)  <= answer
              –––––––––––   –––
                   /  \
  n.1 = (S n.1.1) /    \  [n.1 = (Z)]
                 /      \
     [c3]–––––––––––––   –––[c4]
         (f n.1.1 (F))   (F)
         –––––––––––––   –––
```

Obviously, `[c1] – [c2]` is a loop and there will be no more answers. The only answer goes through `[c0] - [c1]`

Here is a possible URA extension to recognize such idle loops. Remember that URA is bredht-first-search and it has a queue of "pending" nodes of process tree.

```
            ...
            /
          conf1
    [b1]  /  \  [b2]
        ...  ...
        /
     conf2
     /
   ...
   /
 conf3

```

Let current configuration is `conf3` and there are ancestors `conf1` and `conf2` such that `conf1 ~ conf2 ~ conf3` (`~` is a renaming). If there were NO answers (so far) which goes through conf1 AND there are no pending nodes (in the queue) that go through `conf1` but do not go through `conf2`, then we detected a cycle - branch conf3 has no solutions.

Algo:

1. Find `conf1` and `conf2`
2. Check that no (already produced) answer touches `conf1`
3. Check that there is no pending nodes which (a) touches `conf1` (b) but does't touch `conf2`.
4. If all checkes succeeded, then `conf3` is a dead-end.

(TODO: try to prove it in Agda.)

In order to apply this algo, we need following modifications to URA:

* We remember paths of all answers.
