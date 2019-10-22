# Articles

## Implementing an IoC Container in ABAP - Part 1.

We all love a bit of loose coupling, don't we?
Components of a loosely coupled system can easily be changed, the modules depend less on each others intricate details.
But how do we get to loose coupling?
The answer is _dependency inversion_.

### Dependency Inversion Principle

The dependency inversion principle roughly states, that high level components should not depend on low level components, but both should depend on abstractions.
But what does that mean?

Suppose our software consists of two cooperating modules, say _A_ and _B_.
If _A_ uses some functionality of _B_, then _A_ is _dependent_ on _B_ (i.e. _B_ is a dependency of _A_).
Since it is _A_ that is calling _B_, the flow of control also points in the same direction, i.e. from _A_ to _B_.

This relation also means that the functionality implemented in _B_ is abstracted away from the point of view of _A_, thus _B_ is at a lower level of abstraction.
This gets us to the point of having a high level component depending on a low level component.
So how can we solve this situation?

This is where abstractions come into play.
If we insert an abstraction layer (say an interface) between our two components, the direction of the dependencies will change.

_A_ will now depend on the _Interface_ rather than directly on _B_, but _B_ will also depend on _I_.
The flow of control will still go from _A_ to _I_ to _B_, however the direction of the dependency will point _against_ the flow of control.

This way both our components will depend on an abstraction rather than the high level component depending on the low level one.

This creates loose coupling, as we can easily switch _B_ to a different module, as long as we adhere to the contract defined by the abstraction _I_.

### Dependency injection

Dependency injection is the practical manifestation of the dependency inversion principle.
In order for the principle to work in real life, we somehow need to supply _A_ with its dependency _B_, without _A_ knowing the concrete dependency.

If _A_ is using _B_ via an abstraction, but is newing it up _B_ directly then the abstraction doesn't serve it's purpose, as _A_ will still have intimate knowledge about it's dependency being _B_.

To get the dependency in _A_ we can use several techniques, which are collectively called dependency injection.
These (among others) could be:

- Constructor injection
- Property injection (a.k.a. Setter injection)
- Factories
- Inversion of Control containers

#### Constructor injection

When using constructor injection, the dependency is injected via an input parameter of the constructor and then stored as an attribute.

In the above case this would mean something like such:

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

	public DoSomeStuff()
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

So instead of the high level module creating its own dependency, it is supplied upon creation with it.

Since the high level module only knows about the abstraction (note the type of the attribute and the constructor parameter), it will not be tightly coupled to the concrete implementation.
Whichever implementation it is supplied with, it will use that.

#### Property injection

The idea of property injection is quite similar to that of the constructor injection, but in this case the property is not supplied during object creation, rather it can be set using a property or setter method.

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

It is also possible to have a baked in default dependency (as in the above example), which then can be changed in runtime using the property setter.

#### Factories

Another solution is to create a dependency factory, that returns abstractions and use that during object creation.

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

This is really just a more fancy way of doing property injection, as typically the factory should have some way for setting which concrete implementation it should supply.

The advantages above property injection is that this way the logic of figuring out which concrete dependency to supply is localized inside the factory.
Thus if all our modules get their dependencies from the factory it is only one place to change, so that all the modules use a different dependency (e.g. for unit testing).

This also means that it is less flexible as we cannot individually set the dependencies of each high level module (unless we combine it with actual property injection).

#### Inversion of Control containers

The idea behind Inversion of Control containers (or IoC containers for short) is that there is an object that know how to get hold of certain dependencies that will be needed throughout the life of our application.

We configure the IoC container during startup, and it will supply our application with the correct dependencies.

At first glance using an IoC container may seem like black magic but the idea should be quite easy to grasp after the above.

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

The above example uses the syntax of Autofac.
First we configure the IoC container to return an instance of `LowLevelModule` whenever we need an `IDependency`.
Then we just use our program without giving it any more thought.

Upon creating the `HighLevelModule` the IoC container will see that we need an `IDependency` that was not supplied, and will return an instance of `LowLevelModule` as configured.

The advantage of using an IoC container is that the setup part of our application can be very clearly separated from normal operation.

### An ABAP IoC Container

The above mentioned solutions for dependency injection can be easily implemented in ABAP (as in any OO language), save for the usage of IoC containers.
As of now -- at least as far as I know -- there is no standard solution for an IoC container in ABAP.

Since however the ABAP language support runtime type information via the RTTS this can be implemented.
This is what I'll be writing about in the next part of this article.

## Implementing an IoC Container in ABAP - Part 2.

TODO

## Resources:

- https://en.wikipedia.org/wiki/Loose_coupling
- https://en.wikipedia.org/wiki/SOLID
- https://en.wikipedia.org/wiki/Dependency_inversion_principle
- https://en.wikipedia.org/wiki/Dependency_injection
- https://www.youtube.com/watch?v=zHiWqnTWsn4
- https://www.martinfowler.com/articles/injection.html
- https://autofaccn.readthedocs.io/en/latest/getting-started/
- https://app.pluralsight.com/library/courses/using-dependency-injection-on-ramp
- https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenrtti.htm