
function unwrapPtr(value) {
  return value.f.f().pop();
}

function jsNewHash(name) {
  hashName=unwrapPtr(name);
  window[hashName]=new Array();
}

function jsStoreHash(name,key,value) {
  hashName=unwrapPtr(name);
  hashKey=unwrapPtr(key);
  window[hashName][hashKey]=value;
}

function jsReadHash(name,key) {
  hashName=unwrapPtr(name);
  hashKey=unwrapPtr(key);
  return window[hashName][hashKey];
}

function jsInitKeypress(elem, cb) {

  fun = function(x) {B(A(cb,[[0,66],0]));};
  console.log('init');

  elem.addEventListener('keypress', function(e) { fun(e) }, false);
  return true; 
}