import { EditorState } from "@codemirror/state";
import { EditorView, keymap, Decoration, ViewPlugin } from "@codemirror/view";
import {basicSetup} from "codemirror"
import { python } from "@codemirror/lang-python";
import { xcodeLight, xcodeDark } from '@uiw/codemirror-theme-xcode';
import { indentWithTab } from "@codemirror/commands";
import { ayuLight, solarizedLight, noctisLilac } from 'thememirror';
import { vscodeKeymap } from "@replit/codemirror-vscode-keymap";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";


var editor;
const state = EditorState.create({
  doc: "print('Hello, world!')",
  extensions: [
    python(),
    noctisLilac,
    EditorView.theme({
      ".cm-content": {
        fontSize: "11px", // 设置字体大小
      },
      ".cm-comment": {
        color: "#ADD7CB", // 设置注释的颜色
        fontStyle: "italic", // 设置字体样式为斜体
      },
      ".cm-gutter, .cm-lineNumbers": {
        fontSize: "11px", // 设置行号的字体大小
      }
    }, { dark: true }), 
    basicSetup,
    keymap.of([vscodeKeymap, indentWithTab]),
  ]
});

editor = new EditorView({
  state,
  parent: document.getElementById('CodeEditor')
});

window.myCodeEditor = editor;