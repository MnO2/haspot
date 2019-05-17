---
layout: post
title: "Hacking on Swift Compiler (Part 2)"
date: 2016-07-26 17:30
comments: true
categories: 
---
Nothing works better than actually getting a concrete task and work on it. Part 1 is only a brief overlook on the motivation and what has been provided when the Swift is compiled with debug flag on. In Part 2, I find a ticket in Kira that labeled as starter bug and attempt to implement it.

This ticket is about the ambiguity when it comes to tuple as the parameter. It would cause some surprise when the tuple is implicitly expanded and bound with parameter variables. And in the Swift 3, the committee accepted the proposal and decided to impotent extra parenthesis to disambiguate the cases.

To get a clue on what's happening behind the scene, I wrote a snippet source file and use lldb to run swift compiler step by step. Initially I set the breakpoint at typeCheckDecl method and expect it to stop there. However, the program just run to finish when I pressed process launch.

```
$ lldb - swift test.swift
(lldb) target create "swift"
Current executable set to 'swift' (x86_64).
(lldb) settings set - target.run-args "test.swift"
(lldb) b typeCheckDecl
Breakpoint 1: where = swift`swift::TypeChecker::typeCheckDecl(swift::Decl*, bool) + 38 at TypeCheckDecl.cpp:6084, address = 0x00000001012ae1e6
(lldb) process launch
Process 43688 launched: '/Users/mno2/Develop/grabtaxi/build/Ninja-ReleaseAssert+swift-DebugAssert/swift-macosx-x86_64/bin/swift' (x86_64)
Process 43688 stopped
* thread #1: tid = 0x94e653, 0x00007fff5fc01000 dyld`_dyld_start, stop reason = exec
 frame #0: 0x00007fff5fc01000 dyld`_dyld_start
dyld`_dyld_start:
-> 0x7fff5fc01000 <+0>: popq %rdi
 0x7fff5fc01001 <+1>: pushq $0x0
 0x7fff5fc01003 <+3>: movq %rsp, %rbp
 0x7fff5fc01006 <+6>: andq $-0x10, %rsp
(lldb)
```

To figure out why, I set the breakpoint at the obvious entrance module of the whole compiler, which is Driver class. Running step by step through it. I began to identify the keyword like buildCompilation and task->execute, so it looks like when it runs without any flags specified, the default mode is to build up the tasks to be run in another processes so that the compilation could be parallelized.

However, this hinders lldb to trace the actual process doing the typechecking heavylifting since lldb doesn't support follow-fork-mode like gdb does. To figure out what is actually run I need to take a peek what's inside the Job. I set a breakpoint at addJob, and used the handy dump() method as specified in the doc. Then you can see everything that how it should be run with the command line.

```
(lldb) n
Process 5404 stopped
* thread #1: tid = 0x2fe32, 0x00000001000274e0 swiftc`swift::driver::Compilation::addJob(this=0x000000010770cd40, J=<unavailable>) + 64 at Compilation.cpp:94, queue = 'com.apple.main-thread', stop reason = step over
 frame #0: 0x00000001000274e0 swiftc`swift::driver::Compilation::addJob(this=0x000000010770cd40, J=<unavailable>) + 64 at Compilation.cpp:94
 91 Job *Compilation::addJob(std::unique_ptr<Job> J) {
 92 Job *result = J.get();
 93 Jobs.emplace_back(std::move(J));
-> 94 return result;
 95 }
 96
 97 static const Job *findUnfinishedJob(ArrayRef<const Job *> JL,
(lldb) po result->dump()
/Users/Paul/Develop/swift/swiftc/build/Ninja-ReleaseAssert+swift-DebugAssert/swift-macosx-x86_64/bin/swift -frontend -c -primary-file test.swift -target x86_64-apple-macosx10.9 -enable-objc-interop -color-diagnostics -module-name test -o /var/folders/l8/mhy9c9jj72l56gdybs_8l9gc0000gn/T/test-ff159c.o
```

Then I relaunched the lldb with every flags specified.

```
lldb - /Users/Paul/Develop/swift/swiftc/build/Ninja-ReleaseAssert+swift-DebugAssert/swift-macosx-x86_64/bin/swift -frontend -c -primary-file test.swift -target x86_64-apple-macosx10.9 -enable-objc-interop -color-diagnostics -module-name test -o /var/folders/l8/mhy9c9jj72l56gdybs_8l9gc0000gn/T/test-ff159c.o
```

then tried to break at typeCheckDecl as well. This time it succeeds.

```
(lldb) b typeCheckDecl
Breakpoint 3: where = swift`swift::TypeChecker::typeCheckDecl(swift::Decl*, bool) + 38 at TypeCheckDecl.cpp:6094, address = 0x00000001012ca9f6
(lldb) run
Process 5446 launched: '/Users/Paul/Develop/swift/swiftc/build/Ninja-ReleaseAssert+swift-DebugAssert/swift-macosx-x86_64/bin/swift' (x86_64)
Process 5446 stopped
* thread #1: tid = 0x30432, 0x00000001012ca9f6 swift`swift::TypeChecker::typeCheckDecl(this=0x00007fff5fbf6470, D=0x000000010782be00, isFirstPass=true) + 38 at TypeCheckDecl.cpp:6094, queue = 'com.apple.main-thread', stop reason = breakpoint 3.1
 frame #0: 0x00000001012ca9f6 swift`swift::TypeChecker::typeCheckDecl(this=0x00007fff5fbf6470, D=0x000000010782be00, isFirstPass=true) + 38 at TypeCheckDecl.cpp:6094
 6091 }
 6092
 6093 void TypeChecker::typeCheckDecl(Decl *D, bool isFirstPass) {
-> 6094 PrettyStackTraceDecl StackTrace("type-checking", D);
 6095 checkForForbiddenPrefix(D);
 6096 bool isSecondPass =
 6097 !isFirstPass && D->getDeclContext()->isModuleScopeContext();
 ```

 Since the testing snippet is like the following

 ```
 $ cat test.swift
func main() {
 let a: (Int, Int) -> () = { (x, y) in x+y }
}
main()
```

Under the decl, it should match the pattern-binding according to the swift language reference, therefore I set breakpoint on the typeCheckPatternBinding as well

```
(lldb) b typeCheckPatternBinding
Breakpoint 4: where = swift`swift::TypeChecker::typeCheckPatternBinding(swift::PatternBindingDecl*, unsigned int) + 23 at TypeCheckConstraints.cpp:1761, address = 0x00000001012a8a87
(lldb) run
Process 6331 stopped
* thread #1: tid = 0x63b55, 0x00000001012a8a87 swift`swift::TypeChecker::typeCheckPatternBinding(this=0x00007fff5fbf6470, PBD=0x0000000108895410, patternNumber=0) + 23 at TypeCheckConstraints.cpp:1761, queue = 'com.apple.main-thread', stop reason = breakpoint 4.1
 frame #0: 0x00000001012a8a87 swift`swift::TypeChecker::typeCheckPatternBinding(this=0x00007fff5fbf6470, PBD=0x0000000108895410, patternNumber=0) + 23 at TypeCheckConstraints.cpp:1761
 1758 bool TypeChecker::typeCheckPatternBinding(PatternBindingDecl *PBD,
 1759 unsigned patternNumber) {
 1760
-> 1761 Pattern *pattern = PBD->getPattern(patternNumber);
 1762 Expr *init = PBD->getInit(patternNumber);
 1763
 1764 if (!init) {
```

And voila, we got pattern and initializer expression. We can also apply dump method on them to have a peek into what they are.

```
1761 Pattern *pattern = PBD->getPattern(patternNumber);
-> 1762 Expr *init = PBD->getInit(patternNumber);
 1763
 1764 if (!init) {
 1765 PBD->setInvalid();(lldb) po pattern->dump()
(pattern_typed type='(Int, Int) -> ()'
 (pattern_named type='(Int, Int) -> ()' 'a')
 (type_function
 (type_tuple
 (type_ident
 (component id='Int' bind=Swift.(file).Int))
 (type_ident
 (component id='Int' bind=Swift.(file).Int)))
 (type_tuple)))
(lldb) po init->dump()
(closure_expr type='<null>' discriminator=0 single-expression
 (parameter_list
 (parameter "x" type=<null type>)
 (parameter "y" type=<null type>))
 (sequence_expr type='<null>'
 (declref_expr type='<null>' decl=test.(file).func decl.explicit closure discriminator=0.x@test.swift:2:31 specialized=yes)
 (unresolved_decl_ref_expr type='<null>' name=+ specialized=no)
 (declref_expr type='<null>' decl=test.(file).func decl.explicit closure discriminator=0.y@test.swift:2:34 specialized=yes)))
```

so I've zoomed down to the place where we probably need to make the change. We can add some hacks on it to make the ConstraintSystem to derive to different solution I think. I would write Part 3 for my finding afterward.

