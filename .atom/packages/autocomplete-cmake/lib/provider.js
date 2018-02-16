'use babel';

import {filter} from 'fuzzaldrin';
import {dispatch} from './functional';
import cmake_context from './cmake-context';
import commands from './commands';
import * as constants from './constants';
import variables from './variables';

function constants_suggestions(prefix, list) {
    return filter(list, prefix).map((constant) => ({
        text: constant,
        displayText: constant,
        type: 'constant'
    }));
}

function commands_suggestions(prefix) {
    return filter(commands, prefix.toLowerCase()).map((command) => ({
        text: `${command}()`,
        displayText: command,
        type: 'function'
    }));
}

function variables_suggestions(prefix) {
    return filter(variables, prefix.toUpperCase()).map((variable) => ({
        text: variable,
        displayText: variable,
        type: 'variable'
    }));
}

const suggest = dispatch(
    (context, prefix) => {
        if (context === 'find_file' || context === 'find_path') {
            return constants_suggestions(prefix, constants.FindPath);
        }
    },
    (context, prefix) => {
        if (context === 'find_library') {
            return [
                ...constants_suggestions(prefix, constants.FindProgram),
                ...constants_suggestions(prefix, ['NAMES_PER_DIR'])
            ];
        }
    },
    (context, prefix) => {
        if (context === 'find_package') {
            return constants_suggestions(prefix, constants.FindModules);
        }
    },
    (context, prefix) => {
        if (context === 'find_program') {
            return constants_suggestions(prefix, constants.FindProgram);
        }
    },
    (context, prefix) => {
        if (context === 'include') {
            return constants_suggestions(prefix, constants.Modules);
        }
    },
    (context, prefix) => {
        if (context.length > 0) {
            return [
                ...constants_suggestions(prefix, constants.Booleans),
                ...variables_suggestions(prefix)
            ];
        }
    },
    (context, prefix) => commands_suggestions(prefix)
)

export const selector = '.source.cmake';
export const disableForSelector = '.source.cmake .comment';
export const inclusionPriority = 1;

export function getSuggestions({editor, prefix, scopeDescriptor: scope_descriptor}) {
    return suggest(cmake_context(editor, scope_descriptor), prefix);
}

export function onDidInsertSuggestion({editor, suggestion}) {
    if (suggestion && suggestion.type === 'function') {
        editor.moveLeft(1);
    }
}
