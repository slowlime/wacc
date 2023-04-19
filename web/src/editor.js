import ace from 'ace-builds';

function serializeState(editor) {
  return JSON.stringify({
    code: editor.getValue(),
  });
}

function deserializeState(state) {
  try {
    return JSON.parse(state);
  } catch (e) {
    console.error("Could not deserialize state due to the following error:", e);

    return {
      code: '',
    };
  }
}

function storeState(stateKey, editor) {
  window.localStorage.setItem(stateKey, serializeState(editor));
}

function loadState(stateKey, editor) {
  let state = window.localStorage.getItem(stateKey);

  if (!state) {
    return false;
  }

  state = deserializeState(state);
  editor.setValue(state.code, -1);

  return true;
}

export default function setUpEditor(stateKey, elementId) {
  const editor = ace.edit(elementId, {
    useSoftTabs: true,
    enableAutoIndent: true,
    navigateWithinSoftTabs: true,
    tabSize: 2,
  });

  if (!loadState(stateKey, editor)) {
    storeState(stateKey, editor);
  }

  editor.session.on('change', (delta) => {
    storeState(stateKey, editor);
  });

  return editor;
}
