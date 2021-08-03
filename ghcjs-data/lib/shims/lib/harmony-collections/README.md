# Harmony Collections Shim

Use the new __Map__, __Set__, and __WeakMap__ from the upcoming ES6 standard right now! This shim provides full functionality for these collections and delivers the benefits of using them.

## Compatability

Works with IE9+, Chrome, Firefox, Safari, untested in Opera. __IE8 support has been recently added but is experimental.__

## Install/Use

If using node, install via:

    npm install harmony-collections

In the browser, include __harmony-collection.js__ or __harmony-collections.min.js__ and Map, WeakMap, Set, and HashMap will be exposed on the window. (you can also define `window.exports` which will cause them to end up there).

## Overview

ES6 Collections provide a new core weapon to your JS arsenal: objects as keys. This allows you to do the following awesome things: store private data "on" public objects, private properties, secretly "tag" objects, namespace properties, access controlled properties, check object uniqueness in `O(1)` time complexity.

### WeakMap Garbage Collection Semantics

The benefit of using WeakMaps is enhanced garbage collection. In a WeakMap, the only reference created is key -> value, so it's possible for a key/value in a WeakMap to be garbage collected while the WeakMap they're in still exists! Compare this to an Array, where all items in the Array will not be garbage collected as long as the Array isn't. This forces either explicit management of  object lifespans or, more commonly, simply results in memory leaks.

For example, data stored using jQuery.data can never be garbage collected unless explicitly nulled out, because it is stored in a container that strongly references the items held inside. Using a WeakMap, it's possible to associate data with an element and have the data destroyed when the element is -- without memory leaking the element; i.e. `weakmap.set(element, { myData: 'gc safe!' })`. jQuery.data (every library has similar functionality) prevents the *element* from memory leaking by using a numeric id, but this does nothing for the __data__ that is stored.

## Detailed Examples

### Map/WeakMap
```javascript
// reusable storage creator
function createStorage(){
  var store = new WeakMap;
  return function(o){
    var v = store.get(o);
    if (!v) store.set(o, v = {});
    return v;
  };
}

// allows private/namespaced properties for the objects
var _ = createStorage();

functioon Wrapper(element){
  var _element = _(element);
  if (_element.wrapper)
    return _element.wrapper;

  _element.wrapper = this;
  _(this).element = element;
}

Wrapper.prototype = {
  get classes(){
    return [].slice.call(_(this).element.classList);
  },
  set classes(v){
    _(this).element.className = [].concat(v).join(' ');
  }
};
```

### Set
A Set is similar to an Array in what it stores, but different in how. A Set's values are unique. Determining whether an item is in a Set is `O(1)` but `O(n)` for an Array. An example of where this is useful is in implementing `Array.prototype.unique` that works with objects.

Both of the following will output the same result, however the Set version is `O(n)` and the one using indexOf is `O(n^2)`. For an array taking 30 seconds using the set, an __*hour*__ is required for indexOf.

```javascript
function uniqueUsingIndexOf(array){
  return array.filter(function(item, index){
    return array.lastIndexOf(item) > index;
  });
}

function uniqueUsingSet(array){
  var seen = new Set;
  return array.filter(function(item){
    if (!seen.has(item)) {
      seen.add(item);
      return true;
    }
  });
}
```


## API Reference

* Collections may be inherited from. Initialize objects via `[WeakMap|Map|Set].call(obj)`.
* Iteration is insertion ordered.


### WeakMap

__Non-primitives__ are valid keys. Objects, functions, DOM nodes, etc.

WeakMaps require the use of objects as keys; primitives are not valid keys. WeakMaps have no way to enumerate their keys or values. Because of this, the only way to retrieve a value from a WeakMap is to have access to both the WeakMap itself as well as an object used as a key.

* `new WeakMap(iterable)` Create a new WeakMap populated with the iterable. Accepts *[[Key, Value]...]*, *Array*, *Iterable*.
* `WeakMap#set(key, value)` Key must be non-primitive. Returns undefined.
* `WeakMap#get(key)` Returns the value that key corresponds to the key or undefined.
* `WeakMap#has(key)` Returns boolean.
* `WeakMap#delete(key)` Removes the value from the collection and returns boolean indicating if there was a value to delete.


### HashMap

__Primitives__ are valid keys. Exact value is used; e.g. `'0'/-0/0` are all different keys.

HashMap is not standard, but is used to implement Map and is exported as a bonus. Has the same API as Map except it only allows primitive keys.

* `new HashMap(iterable)` Create a new HashMap populated with the iterable. Accepts *[[Key, Value]...]*, *Iterable*.
* `HashMap#set(key, value)` Key must be primitive. Returns undefined.
* `HashMap#get(key)` Returns the value the key corresponds to or undefined.
* `HashMap#has(key)` Returns boolean.
* `HashMap#delete(key)` Removes the value from the collection and returns boolean indicating if there was a value to delete.
* `HashMap#size()` Returns the number of items in the collection.
* `HashMap#forEach(callback, context)` Loop through the collection raising callback for each.


### Map

__All possible values__ are valid keys, including -0, undefined, null, and NaN.

Maps do not have the same garbage collection benefits that WeakMaps do, but instead are iterable and also accept primitive keys. This means any value can be a Map key.

* `new Map(iterable)` Create a new Map populated with the iterable. Accepts *[[Key, Value]...]*, *Array*, *Iterable*.
* `Map#set(key, value)` Key is any value including objects. Returns undefined.
* `Map#get(key)` Returns the value the key maps to or undefined.
* `Map#has(key)` Returns boolean.
* `Map#delete(key)` Removes the key and value from the collection if found. Returns true.
* `Map#size()` Returns the number of items in the collection.
* `Map#forEach(callback, context)` Loop through the collection raising callback for each.


### Set

Sets are similar to arrays but enforce uniqueness of values. Adding the same value twice will only result in one being added to the set.

* `new Set(iterable)` Create a new Set populated with the iterable. Accepts *Array*, *Iterable*.
* `Set#add(value)` Inserts a value of any type into the set if it's not already in the set.
* `Set#has(value)` Returns boolean.
* `Set#delete(value)` Removes the value from the collection and returns boolean indicating if there was a value to delete.
* `Set#size()` Returns the number of items in the collection.
* `Set#forEach(callback, context)` Loop through the collection raising callback for each.



## License

(The MIT License)
Copyright (c) 2012 Brandon Benvie <http://bbenvie.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
(the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included with all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
