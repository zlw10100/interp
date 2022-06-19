
# interp

interp是用racket编写的元语言解释器。项目目的是站在“语言设计者”的层次去思考编程语言的 功能和使用。得益于lisp的简单语法，interp无需考虑parser，从而更注重interpreter的核心逻辑。该项目已 经实现了通用编程语言必不可少的基础特性，并且测试通过。interp代码逻辑结构简单，容易扩充新的功能。

## 快速开始
### 安装并引入
安装racket。

### 执行测试
运行test.rkt。

## 支持功能
- interp使用CPS写法实现，可以简单自由的操作程序上下文continuation以支持各类并发、跳转机制。
- 支持first class函数定义、变量定义、函数调用、递归、多元分支、循环、尾递归优化、词法作用域、赋值、逻辑操作、算术运算、列表操作、模式匹配等。
- 支持lambda calculus、Y combinator、call/cc、reset/shift、quote/quasiquote、eval/namespace、let/let*/letrec等。
