# tsg-debugger
TSG Debugger

## Usage: Haskell

```
ghci TsgDebugger.hs
r2
```

`r2` is a shortcut for sum(110100, 10010). In general you can call `runner $ mkEvalState <YOUR-PROGRAM> <YOUR-ARGUMENTS>`

### Commands
- <p>: print program
- <b x:Int>: toggle breakpoint for line x
- <r>: run
- <n>: do 1 step
- <t>: toggle long/short vars
- <s x:Int>: do x steps
- <j x:Int>: run untill line x
- <q>, <e>: quit

## Usage: Emacs
- run ghci in `*shell*<1>` (or manually `*ghci-buf*` in `tsg-debugger.el`)
- eval `tsg-debugger.el` (e.g. open it and run `eval-buffer`)
- eval `tsg/repl` (e.g. press `s-l`)

