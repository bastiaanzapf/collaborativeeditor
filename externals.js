
function jsNewHash(name) {
  window[name]=new Array();
}

function jsStoreHash(name,key,value) {
  window[name][key]=value;
}

function jsReadHash(name,key) {
  x=window[name][key];
  if (typeof x == 'undefined')
    return [0]
  else
    return [1,x]
}

function jsInitKeypress(elem, cb) {

  fun = function(x) {B(A(cb,[[0,66],0]));};
  console.log('init');

  elem.addEventListener('keypress', function(e) { fun(e) }, false);
  return true; 
}

function jsTest(test) {
console.log(test);
return [0]
}

// Handle caret

function jsCaretPosition(o) {
    var sel = window.getSelection();
    if (sel.anchorNode.parentNode==o)
       return [1,[0,sel.anchorOffset]];
    else
       return [0];
}

function jsCharacterLeftOfCaret(o) {

    var sel = window.getSelection();
    if (sel.anchorNode.parentNode==o) {
       var pos = sel.anchorOffset; 
       if (pos>0)
           return [1,[0,o.textContent.charCodeAt(pos-1)]];
       else
           return [0]
    } else
       return [0];
}

function jsTextLength(o) {
   return o.textContent.length;
}

function jsSetCaretPosition(o,pos) {
   var range = document.createRange();
   range.setStart(o.firstChild, pos);
   range.setEnd(o.firstChild, pos);

   var selection = window.getSelection();
   selection.removeAllRanges();
   selection.addRange(range);
}

function jsInsertAt(o,pos,chr) {
   oldlength=o.textContent.length;
   old_pos=jsCaretPosition(o);

   o.textContent = o.textContent.substring(0,pos) + String.fromCharCode(chr) + o.textContent.substring(pos);

   if (old_pos[0]==1 && old_pos > pos) {
      jsSetCaretPosition(o,old_pos[1][1]+1);
   }

}

function jsDeleteAt(o,pos,chr) {
   oldlength=o.textContent.length;
   old_pos=jsCaretPosition(o);

   o.textContent = o.textContent.substring(0,pos-1) + o.textContent.substring(pos);

   if (old_pos[0]==1 && old_pos > pos) {
      jsSetCaretPosition(o,old_pos[1][1]-1);
   } else {
      jsSetCaretPosition(o,old_pos[1][1]);
   }

}