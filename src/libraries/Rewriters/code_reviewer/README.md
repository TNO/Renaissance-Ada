# Code Reviewer

Automatic Code Reviewer of Ada Code

This tools produces patches that contain changes to your code 
that you might consider improvements.

Note that we don't recommend accepting all changes blindly.

## Example

The following snippet
```diff
   function Release_Only (Mode : Operation_Mode) return Boolean is
-     (case Mode is when Release_Size_Mode | Release_Speed_Mode => True, when others => False);
+     (Mode in Release_Size_Mode | Release_Speed_Mode);
```
is produced using the rewriter
that replaces every case expression that adheres to the pattern
```ada
case $S_Expr is 
   when $M_Values => $S_Val_In, 
   when others => $S_Val_Out
```
i.e., a **binary** case expression with an **others** alternative,
by the equivalent if expression, that adheres to the pattern
```ada
if ($S_Expr) in $M_Values 
   then $S_Val_In 
   else $S_Val_Out
```
The rewritten Ada code is further simplified using 
the rewriter that simplifies if expressions that match
```ada
if $S_Cond then true else false
```
with the condition `$S_Cond`
and
the rewriter for minimal parenthesis that removes unnecessary parenthesis.

# Warning

Note that some changes to your code might be incorrect.
Recently, we started adding semantic checks to prevent incorrect changes as much as possible,
but probably we will never be able to guarantee that all rewrites are correct.

