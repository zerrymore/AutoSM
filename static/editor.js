// editors.js
var sapicEditor = ace.edit("sapicEditor");
sapicEditor.setTheme("ace/theme/dreamweaver");
sapicEditor.session.setMode("ace/mode/rust");
sapicEditor.session.setTabSize(2);
sapicEditor.getSession().removeAllListeners("changeAnnotation");
sapicEditor.getSession().setAnnotations([]);

var logEditor = ace.edit("logEditor");
logEditor.setTheme("ace/theme/sqlserver");
logEditor.session.setMode("ace/mode/julia");
logEditor.session.setTabSize(2);
logEditor.getSession().removeAllListeners("changeAnnotation");
logEditor.getSession().setAnnotations([]);


function setLabel(slider) {
    var value = slider.value; 
    document.getElementById('temp_value').textContent = value; 
}


function updateEditorContent(editor, newContent) {
  var editor = ace.edit(editor);
    editor.getSession().setValue(newContent);
}


