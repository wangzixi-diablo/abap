# Jerry's SCN blog
* [Implement CGLIB in ABAP](https://blogs.sap.com/2017/01/27/implement-cglib-in-abap)
* [Create dynamic proxy persistently in Java and ABAP](https://blogs.sap.com/2017/01/28/create-dynamic-proxy-persistently-in-java-and-abap)
* [Simulate Mockito in ABAP](https://blogs.sap.com/2017/02/02/simulate-mockito-in-abap)
* [A useful Java CSDN blog of CGLIB](http://blog.csdn.net/dongnan591172113/article/details/42170871)

# Java CGLIB
cglib是针对类来实现代理的，他的原理是对指定的目标类生成一个子类，并覆盖其中方法实现增强，但因为采用的是继承，所以不能对final修饰的类进行代理。

CGLIB(Code Generation Library)是一个开源项目，是一个强大的，高性能，高质量的Code生成类库，它可以在运行期扩展Java类与实现Java接口。Hibernate用它来实现PO(Persistent Object 持久化对象)字节码的动态生成。

[Spring AOP原理为什么用2种实现方式?JDKProxy和Cglib](https://www.zhihu.com/question/34301445)
为什么不都使用Cgilb代理：
因为JDK动态代理不依赖其他包，Cglib需要导入ASM包，对于简单的有接口的代理使用JDK动态代理可以少导入一个包。

[Cglib为什么不能完全替换 动态代理](http://flyfoxs.iteye.com/blog/2074188)