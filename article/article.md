# Implementing an IoC Container in ABAP

## Preface

The following is a two part article describing my adventures of creating a naive implementation of an inversion of control container in ABAP.

The first part will focus on the Dependency Inversion Principle and ways of achieving it.
The second part will explain the ABAP implementation of the IoC container.

## Part 1 - Dependency inversion

The key to a modular application is [loose coupling][loosecoupling].
Loosely coupled components usually makes changes easier potentially reducing maintenance effort while also allowing separation of concerns and isolation for unit testing.

The way to achieve loose coupling in OO applications is to adhere to the [Dependency Inversion Principle][dip] (the _D_ of _[SOLID][solid]_).

### Dependency Inversion Principle

The dependency inversion principle roughly states that:

> high level components should not depend on low level components, but both should depend on abstractions.

Suppose our software consists of two cooperating modules, say module _A_ and _B_.
If _A_ uses some functionality of _B_, then _A_ is _dependent_ on _B_ (i.e. _B_ is a dependency of _A_).

This relation also means that functionality implemented in _B_ is abstracted away from the point of view of _A_, thus _B_ is at a lower level of abstraction.
Since _A_ is calling _B_ the flow of control also points in the same direction as the dependency, i.e. from _A_ to _B_.

![Dependent Modules](./img/dependency_inversion.png)

To resolve this dependent state between _A_ and _B_ we can introduce an abstraction layer (e.g. an interface) between the two components to flip the direction of the dependency.

By inserting the interface between the two components _A_ will now depend on _I_ (the abstraction) rather than directly on _B_ while _B_ will also depend on _I_ (implementing the interface in this case).
The flow of control will still go from _A_ to _I_ to _B_, however the direction of the dependency will point _against_ the flow of control in the relation of _I_ to _B_.

This way both our components will depend on an abstraction rather than the high level component depending on the low level one.
This creates loose coupling, as we now can easily switch _B_ to a different module, as long as we adhere to the contract defined by the abstraction _I_.

![Dependent Modules with Abstraction](./img/dependency_inversion_flipped.png)

### Dependency injection

[Dependency injection][dependency_injection] is a way of putting the Dependency Inversion Principle into practice.
In order for our software to work we need to supply _A_ with its dependency _B_ without _A_ knowing the concrete dependency.

If _A_ is using _B_ via an abstraction, but it is newing up _B_ directly then the abstraction doesn't serve it's purpose, as _A_ will still be tightly coupled to _B_.

To get the dependency to _A_ we can use several techniques, which are collectively known as dependency injection.
These (among others) could be:

- Constructor injection
- Property injection (a.k.a. Setter injection)
- Factories
- Inversion of Control containers

For the examples below I will use some variation of the following classes:

![Dependency injection](./img/modules.png)

#### Constructor injection

When using constructor injection, the dependency is injected via a parameter through the constructor and stored as an attribute.
The following code example shows this in C#.

```C#
public interface IDependency
{
	void LowLevelStuff();
}

public class HighLevelModule
{
	private IDependency _myLowLevelModule;

	public HighLevelModule(IDependency lowLevelModule)
	{
		_myLowLevelModule = lowLevelModule;
	}

	public void DoSomeStuff()
	{
		Console.WriteLine("I'll do some stuff");
		_myLowLevelModule.LowLevelStuff();
	}
}

public class LowLevelModule: IDependency
{
	public void LowLevelStuff()
	{
		Console.WriteLine("In LowLevelModule");
	}
}

public class AnotherLowLevelModule: IDependency
{
	public void LowLevelStuff()
	{
		Console.WriteLine("In AnotherLowLevelModule");
	}
}

public class Program
{
	static void Main(string[] args)
	{
		var lowLevelModule = new LowLevelModule();
		var highLevelModule = new HighLevelModule(lowLevelModule);
		highLevelModule.DoSomeStuff();
	}
}
/*
This code example produces the following results:

I'll do some stuff
In LowLevelModule
*/
```

Instead of the high level module creating its own dependency, it is supplied upon creation with it.

Since the high level module only knows about the abstraction (note the type of the attribute and the constructor parameter), it will not be tightly coupled to the concrete implementation.

#### Property injection

The idea of property injection is quite similar to that of the constructor injection, but instead of supplying the dependency during object creation, it can be set using a property or setter method.

Reworking the above example:

```C#
...
public class HighLevelModule
{
	private IDependency _myLowLevelModule;
	public IDependency MyLowLevelModule { set => _myLowLevelModule = value; }

	public HighLevelModule()
	{
		// Use LowLevelModule by default
		_myLowLevelModule = new LowLevelModule();
	}
...
}
...

public class Program
{
	static void Main(string[] args)
	{
		var highLevelModule = new HighLevelModule();
		highLevelModule.DoSomeStuff();

		var anotherLowLevelModule = new AnotherLowLevelModule();
		highLevelModule.MyLowLevelModule = anotherLowLevelModule;
		highLevelModule.DoSomeStuff();
	}
}
/*
This code example produces the following results:

I'll do some stuff
In LowLevelModule
I'll do some stuff
In AnotherLowLevelModule
*/
```

Note that the property is public so it can be changed from outside the class.

It is also possible to have a baked in default dependency (as in the code above), which then can be changed during runtime using the property setter.

#### Factories

Another solution is to have a dependency factory, that supply an abstraction and use that in the constructor.

```C#
...
public static class LowLevelModuleFactory {
	public static IDependency CreateModule()
	{
		return new LowLevelModule();
	}
}

public class HighLevelModule
{
	private IDependency _myLowLevelModule;

	public HighLevelModule()
	{
		// Use LowLevelModule by default
		_myLowLevelModule = LowLevelModuleFactory.CreateModule();
	}
...
}
...
public class Program
{
	static void Main(string[] args)
	{
		var highLevelModule = new HighLevelModule();
		highLevelModule.DoSomeStuff();
	}
}
/*
This code example produces the following results:

I'll do some stuff
In LowLevelModule
*/
```

This is a solution somewhere between a constructor injection and a property injection.
It is typically also possible to configure the factory which concrete implementation it should create.

The advantage of using factories is that the object creation is localized inside the factory instead of being scatter throughout the application (see also [Factory Method][factory]).

#### Inversion of Control containers

The idea behind Inversion of Control containers (or IoC containers for short) is to have an object that knows how to get hold of those dependencies that will be needed throughout the life of our application.

We configure the IoC container upon startup telling which concrete implementations it should supply and then leave the object creation to the IoC container.
The following example shows a way of doing it in C# using the [Autofac][autofac] NuGet Package.

```C#
...
public class Program
{
	private static IContainer Container { get; set; }

	static void Main(string[] args)
	{
		var builder = new ContainerBuilder();
		builder.RegisterType<LowLevelModule>().As<IDependency>();
		Container = builder.Build();

		var highLevelModule = new HighLevelModule();
		highLevelModule.DoSomeStuff();
	}
}
```

The container is configured to return `LowLevelModule` instances for `IDependency`.
Upon creating the `HighLevelModule` the IoC container will realize that an `IDependency` is needed which was not supplied and supply the constructor with a `LowLevelModule` as configured.

### An ABAP IoC Container

The constructor injection, property injection and factories can easily be implemented in ABAP as in any other OO language.
However -- as far as I know -- there is currently no standard solution for an IoC container in ABAP.

Since the ABAP language supports runtime type information via the [RTTS][rtts] it seems possible to implement an IoC container and in the second part of this article I will describe one way of doing it.

## Part 2 - The ABAP IoC Container

TODO

## Resources

- https://www.youtube.com/watch?v=zHiWqnTWsn4
- https://www.martinfowler.com/articles/injection.html
- https://app.pluralsight.com/library/courses/using-dependency-injection-on-ramp

[loosecoupling]: https://en.wikipedia.org/wiki/Loose_coupling
[dip]: https://en.wikipedia.org/wiki/Dependency_inversion_principle
[solid]: https://en.wikipedia.org/wiki/SOLID
[dependency_injection]: https://en.wikipedia.org/wiki/Dependency_injection
[factory]: https://en.wikipedia.org/wiki/Factory_method_pattern
[autofac]: https://autofac.org/
[rtts]: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenrtti.htm