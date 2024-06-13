"use strict";


let admin;
let name;

name = "john";

admin = name;

alert(admin);

// best practices:
/*

0. javascript is dynamically typed (can change type of variables)
1. use strict mode
2. use let and not var? (options in general var/let/const)
3. camelCase for variables
4. const CAP_CONST for "hard-coded" values and const someConst for values not know before execution
5. data types - promitives: number, bigint, boolean, null, undefined, symbol. everythin that is not primitve is an object (including functions)
6. can use `blah ${value}` (like f-string in python), Another advantage of using backticks is that they allow a string to span multiple lines
7. modal window - alert, prompt, confirm
8. use ; at end of statments (even if not necessary-be on safe side)
9. putting +x converts to Number, same as doing Number(x)
10. a = (1+2, 3+4), evalutes all but returns to a only 3+4 (comma operator)
11. == checks equality with type conversion, === checks strict equaltiy, no conversions (normal ==)
12. "2" > "12" --> true as we compare in lexographical order and "2" > "1"
13. short if statement: let result = condition ? value1 : value2; example: let accessAllowed = (age > 18) ? true : false; (in this case same as let accessAllowed = age > 18;)
14. !!null = false  // converts to boolean value
15. || logical or in javescript converts value to bool than returns the original value if evaluted to true
16. && return the first falsy value (alert(1 && null && 2); returns null)
17. The nullish coalescing operator is written as two question marks ??. (short way to  choose the first "defined" value from a list)
As it treats null and undefined similarly, we’ll use a special term here, in this article. For brevity, we’ll say that a value is “defined” when it’s neither null nor undefined.
The result of a ?? b is:
if a is defined, then a,
if a isn’t defined, then b.
18. for (;;) is an infinite loop
19. The break <labelName> statement in the loop below breaks out to the label:

outer: for (let i = 0; i < 3; i++) {

  for (let j = 0; j < 3; j++) {

    let input = prompt(`Value at coords (${i},${j})`, '');

    // if an empty string or canceled, then break out of both loops
    if (!input) break outer; // (*)

    // do something with the value...
  }
}

alert('Done!');

20. switch case in javascript:
switch(a) {
	case 2:
		...
		break;
	case 5:
		...
		break;
	default:
		...
}

can group cases:
case 2:
case 5:
	...
	break;

21. Function Declaration:
function sayHi() {
  alert( "Hello" );
}

Function Expression:
let sayHi = function() {
  alert( "Hello" );
};

22. callbacks
23. arrow functions - let func = (arg1, arg2, ..., argN) => {expression};
24. code style: functions fist in the file, then code. npm install -g eslint, Create a config file named .eslintrc in the root of your JavaScript project (in the folder that contains all your files).
Install/enable the plugin for your editor that integrates with ESLint. The majority of editors have one
/**
 * Returns x raised to the n-th power.
 *
 * @param {number} x The number to raise.
 * @param {number} n The power, must be a natural number.
 * @return {number} x raised to the n-th power.
 */
//function pow(x, n) {
  //...
//}
/*

25. Mocha - testing framework
describe("pow", function() {

  it("raises to n-th power", function() {
    assert.equal(pow(2, 3), 8);
    assert.equal(pow(3, 4), 81);
  });

});

26. As programmers, we’d like to use most recent features. The more good stuff – the better!

On the other hand, how to make our modern code work on older engines that don’t understand recent features yet?

There are two tools for that:

Transpilers.
Polyfills.

27. Objects:
let user = new Object(); // "object constructor" syntax
let user = {};  // "object literal" syntax
Usually, the figure brackets {...} are used. That declaration is called an object literal.

computes proprties: let fruit = prompt("Which fruit to buy?", "apple");

let bag = {
  [fruit]: 5, // the name of the property is taken from the variable fruit
};

alert( bag.apple ); // 5 if fruit="apple"

//

function makeUser(name, age) {
  return {
    name, // same as name: name
    age,  // same as age: age
    // ...
  };
}

//

let user = { name: "John", age: 30 };

alert( "age" in user ); // true, user.age exists
alert( "blabla" in user ); // false, user.blabla doesn't exist

//


for in:

for (key in object) {
  // executes the body for each key among object properties
}


//

So, to fix the issue with the phone codes, we can “cheat” by making the codes non-integer. Adding a plus "+" sign before each code is enough.

Like this:

let codes = {
  "+49": "Germany",
  "+41": "Switzerland",
  "+44": "Great Britain",
  // ..,
  "+1": "USA"
};

for (let code in codes) {
  alert( +code ); // 49, 41, 44, 1
}

//

One of the fundamental differences of objects versus primitives is that objects are stored and copied “by reference”, whereas primitive values: strings, numbers, booleans, etc – are always copied “as a whole value”.

//


An important side effect of storing objects as references is that an object declared as const can be modified

//

Object.assign(dest, ...sources)
The first argument dest is a target object.
Further arguments is a list of source objects.
It copies the properties of all source objects into the target dest, and then returns it as the result.


To make a “real copy” (a clone) we can use Object.assign for the so-called “shallow copy” (nested objects are copied by reference) or a “deep cloning” function structuredClone or use a custom cloning implementation, such as _.cloneDeep(obj).

//

methods:

let user = {
  name: "John",
  age: 30
};

user.sayHi = function() {
  alert("Hello!");
};


function sayHi() {
  alert("Hello!");
}


// these objects do the same

user = {
  sayHi: function() {
    alert("Hello");
  }
};

// method shorthand looks better, right?
user = {
  sayHi() { // same as "sayHi: function(){...}"
    alert("Hello");
  }
};

The value of this is evaluated during the run-time, depending on the context.

For instance, here the same function is assigned to two different objects and has different “this” in the calls:

let user = { name: "John" };
let admin = { name: "Admin" };

function sayHi() {
  alert( this.name );
}

// use the same function in two objects
user.f = sayHi;
admin.f = sayHi;


//

constructor:

function User(name) {
  this.name = name;
  this.isAdmin = false;
}

let user = new User("Jack");

So let user = new User("Jack") gives the same result as:

let user = {
  name: "Jack",
  isAdmin: false
};


function User(name) {
  this.name = name;

  this.sayHi = function() {
    alert( "My name is: " + this.name );
  };
}

//

optional chaining:
The optional chaining ?. stops the evaluation if the value before ?. is undefined or null and returns undefined.

Further in this article, for brevity, we’ll be saying that something “exists” if it’s not null and not undefined.

In other words, value?.prop:

works as value.prop, if value exists,
otherwise (when value is undefined/null) it returns undefined.
Here’s the safe way to access user.address.street using ?.:

let user = {}; // user has no address

alert( user?.address?.street ); // undefined (no error)

/
For example, ?.() is used to call a function that may not exist.
/

/
The ?.[] syntax also works, if we’d like to use brackets [] to access properties instead of dot .. Similar to previous cases, it allows to safely read a property from an object that may not exist.
/

//

A “symbol” represents a unique identifier.

let id1 = Symbol("id");
let id2 = Symbol("id");

alert(id1 == id2); // false

 we really want to show a symbol, we need to explicitly call .toString() on it, like here:

let id = Symbol("id");
alert(id.toString()); // Symbol(id), now it works
Or get symbol.description property to show the description only:

let id = Symbol("id");
alert(id.description); // id

(didn't really go over symbols)

28. DATA TYPES:

let billion = 1_000_000_000;
Here the underscore _ plays the role of the “syntactic sugar”, it makes the number more readable. The JavaScript engine simply ignores _ between digits, so it’s exactly the same one billion as above.

//

Two dots to call a method
Please note that two dots in 123456..toString(36) is not a typo. If we want to call a method directly on a number, like toString in the example above, then we need to place two dots .. after it.

If we placed a single dot: 123456.toString(36), then there would be an error, because JavaScript syntax implies the decimal part after the first dot. And if we place one more dot, then JavaScript knows that the decimal part is empty and now goes the method.

Also could write (123456).toString(36).

//

But for arrays there is another form of loop, for..of:

let fruits = ["Apple", "Orange", "Plum"];

// iterates over array elements
for (let fruit of fruits) {
  alert( fruit );
}


The length property automatically updates when we modify the array. To be precise, it is actually not the count of values in the array, but the greatest numeric index plus one:

let arr = [1, 2, 3, 4, 5];

arr.length = 2; // truncate to 2 elements
alert( arr ); // [1, 2]

arr.length = 5; // return length back
alert( arr[3] ); // undefined: the values do not return
So, the simplest way to clear the array is: arr.length = 0;.


Arrays have their own implementation of toString method that returns a comma-separated list of elements.

For instance:

let arr = [1, 2, 3];

alert( arr ); // 1,2,3
alert( String(arr) === '1,2,3' ); // true


The arr.splice method is a Swiss army knife for arrays. It can do everything: insert, remove and replace elements.

The syntax is:

arr.splice(start[, deleteCount, elem1, ..., elemN])


The arr.forEach method allows to run a function for every element of the array.

The syntax:

arr.forEach(function(item, index, array) {
  // ... do something with an item
});
["Bilbo", "Gandalf", "Nazgul"].forEach((item, index, array) => {
  alert(`${item} is at index ${index} in ${array}`);
});


The arr.map method is one of the most useful and often used.

It calls the function for each element of the array and returns the array of results.

The syntax is:

let result = arr.map(function(item, index, array) {
  // returns the new value instead of item
});
For instance, here we transform each element into its length:

let lengths = ["Bilbo", "Gandalf", "Nazgul"].map(item => item.length);
alert(lengths); // 5,7,6

//

et student = {
  name: 'John',
  age: 30,
  isAdmin: false,
  courses: ['html', 'css', 'js'],
  spouse: null
};

let json = JSON.stringify(student);

alert(typeof json); // we've got a string!

alert(json);
/* JSON-encoded object:
{
  "name": "John",
  "age": 30,
  "isAdmin": false,
  "courses": ["html", "css", "js"],
  "spouse": null
}
*/
/*

29. advanced:

function sumAll(...args) { // args is the name for the array
  let sum = 0;

  for (let arg of args) sum += arg;

  return sum;
}

//


30. object proprties configs:

The method Object.getOwnPropertyDescriptor allows to query the full information about a property.

The syntax is:

let descriptor = Object.getOwnPropertyDescriptor(obj, propertyName);


(didnt really go over)

//

getter and setter:

let user = {
  name: "John",
  surname: "Smith",

  get fullName() {
    return `${this.name} ${this.surname}`;
  },

  set fullName(value) {
    [this.name, this.surname] = value.split(" ");
  }
};

// set fullName is executed with the given value.
user.fullName = "Alice Cooper";

alert(user.name); // Alice
alert(user.surname); // Cooper


31. proto:

In JavaScript, objects have a special hidden property [[Prototype]] (as named in the specification), that is either null or references another object. That object is called “a prototype”:

(deprecated);
Setting or reading the prototype with obj.__proto__ is considered outdated and somewhat deprecated (moved to the so-called “Annex B” of the JavaScript standard, meant for browsers only).

The modern methods to get/set a prototype are:

Object.getPrototypeOf(obj) – returns the [[Prototype]] of obj.
Object.setPrototypeOf(obj, proto) – sets the [[Prototype]] of obj to proto.
The only usage of __proto__, that’s not frowned upon, is as a property when creating a new object: { __proto__: ... }.

Although, there’s a special method for this too:

Object.create(proto[, descriptors]) – creates an empty object with given proto as [[Prototype]] and optional property descriptors.

prototype!!!!!!

prototype is a property of a function object in JavaScript.
When a function is created, it automatically gets a prototype property, which is an object.
This prototype object is used as the prototype for all objects created using that function as a constructor with the new keyword.
The prototype property is used to define shared methods and properties that will be inherited by all instances created from the constructor function.


32. classes:

The basic syntax is:

class MyClass {
  // class methods
  constructor() { ... }
  method1() { ... }
  method2() { ... }
  method3() { ... }
  ...
}

(syntqatic sugar that uses prototype)


//

inheritance- extends:

class Rabbit extends Animal {
  hide() {
    alert(`${this.name} hides!`);
  }
}

let rabbit = new Rabbit("White Rabbit");

rabbit.run(5); // White Rabbit runs with speed 5.
rabbit.hide(); // White Rabbit hides!


With constructors it gets a little bit tricky.

Until now, Rabbit did not have its own constructor.

According to the specification, if a class extends another class and has no constructor, then the following “empty” constructor is generated:

class Rabbit extends Animal {
  // generated for extending classes without own constructors
  constructor(...args) {
    super(...args);
  }
}


// static

We can also assign a method to the class as a whole. Such methods are called static.

In a class declaration, they are prepended by static keyword, like this:

class User {
  static staticMethod() {
    alert(this === User);
  }
}

User.staticMethod(); // true

// protected

Protected properties are usually prefixed with an underscore _.

That is not enforced on the language level, but there’s a well-known convention between programmers that such properties and methods should not be accessed from the outside.

So our property will be called _waterAmount:

class CoffeeMachine {
  _waterAmount = 0;

  set waterAmount(value) {
    if (value < 0) {
      value = 0;
    }
    this._waterAmount = value;
  }

  get waterAmount() {
    return this._waterAmount;
  }

  constructor(power) {
    this._power = power;
  }

}

// create the coffee machine
let coffeeMachine = new CoffeeMachine(100);

// add water
coffeeMachine.waterAmount = -10; // _waterAmount will become 0, not -10


// private (new. actually private opposed to protected)

There’s a finished JavaScript proposal, almost in the standard, that provides language-level support for private properties and methods.

Privates should start with #. They are only accessible from inside the class.

For instance, here’s a private #waterLimit property and the water-checking private method #fixWaterAmount:

class CoffeeMachine {
  #waterLimit = 200;

  #fixWaterAmount(value) {
    if (value < 0) return 0;
    if (value > this.#waterLimit) return this.#waterLimit;
  }

  setWaterAmount(value) {
    this.#waterLimit = this.#fixWaterAmount(value);
  }

}

let coffeeMachine = new CoffeeMachine();

// can't access privates from outside of the class
coffeeMachine.#fixWaterAmount(123); // Error
coffeeMachine.#waterLimit = 1000; // Error


//

obj instanceof Class
It returns true if obj belongs to the Class or a class inheriting from it.

//


In JavaScript we can only inherit from a single object. There can be only one [[Prototype]] for an object. And a class may extend only one other class.

But sometimes that feels limiting. For instance, we have a class StreetSweeper and a class Bicycle, and want to make their mix: a StreetSweepingBicycle.

Or we have a class User and a class EventEmitter that implements event generation, and we’d like to add the functionality of EventEmitter to User, so that our users can emit events.

There’s a concept that can help here, called “mixins”.

As defined in Wikipedia, a mixin is a class containing methods that can be used by other classes without a need to inherit from it.

In other words, a mixin provides methods that implement a certain behavior, but we do not use it alone, we use it to add the behavior to other classes.


33. erros:

try {}
catch(err) {}


34. promises:

let promise = new Promise(function(resolve, reject) {
  // executor (the producing code, "singer")
});

So to summarize: the executor runs automatically and attempts to perform a job. When it is finished with the attempt, it calls resolve if it was successful or reject if there was an error.

The most important, fundamental one is .then.

The syntax is:

// resolve runs the first function in .then
promise.then(
  result => alert(result), // shows "done!" after 1 second
  error => alert(error) // doesn't run
);
/*

//

let promise = fetch(url);
This makes a network request to the url and returns a promise. The promise resolves with a response object when the remote server responds with headers, but before the full response is downloaded.

//

let promise = Promise.all(iterable);
Promise.all takes an iterable (usually, an array of promises) and returns a new promise.

The new promise resolves when all listed promises are resolved, and the array of their results becomes its result.

35. async await:
The word “async” before a function means one simple thing: a function always returns a promise. Other values are wrapped in a resolved promise automatically.

he keyword await makes JavaScript wait until that promise settles and returns its result

example-
async function showAvatar() {

async function f() {

  try {
    let response = await fetch('http://no-such-url');
  } catch(err) {
    alert(err); // TypeError: failed to fetch
  }
}

f();

36. generator - function*:

function* generateSequence() {
  yield 1;
  yield 2;
  return 3;
}

let generator = generateSequence();

let one = generator.next();

//

The yield* directive delegates the execution to another generator. This term means that yield* gen iterates over the generator gen and transparently forwards its yields outside. As if the values were yielded by the outer generator.




*/