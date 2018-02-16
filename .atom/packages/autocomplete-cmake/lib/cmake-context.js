'use babel';

import {Range} from 'atom';

function skip(i, predicate) {
    while (i >= 0 && predicate(i)) {
        i--;
    }
    return i;
}

export default function(editor, scope_descriptor) {
    if (scope_descriptor.scopes.length > 1) {
        const range = new Range([0, 0], editor.getCursorBufferPosition());
        const text = editor.getTextInRange(range);
        const last = skip(
            skip(text.length - 1, index => text.charAt(index) !== '(') - 1,
            index => /\s/.test(text.charAt(index))
        ) + 1;
        const first = skip(
            last - 1,
            index => !/\s/.test(text.charAt(index))
        );
        return text.substring(first, last).trim();
    }
    return '';
}
