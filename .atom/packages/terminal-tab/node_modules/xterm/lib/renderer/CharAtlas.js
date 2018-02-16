"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var CharAtlasGenerator_1 = require("../shared/CharAtlasGenerator");
exports.CHAR_ATLAS_CELL_SPACING = 1;
var charAtlasCache = [];
function acquireCharAtlas(terminal, colors, scaledCharWidth, scaledCharHeight) {
    var newConfig = generateConfig(scaledCharWidth, scaledCharHeight, terminal, colors);
    for (var i = 0; i < charAtlasCache.length; i++) {
        var entry = charAtlasCache[i];
        var ownedByIndex = entry.ownedBy.indexOf(terminal);
        if (ownedByIndex >= 0) {
            if (configEquals(entry.config, newConfig)) {
                return entry.bitmap;
            }
            else {
                if (entry.ownedBy.length === 1) {
                    charAtlasCache.splice(i, 1);
                }
                else {
                    entry.ownedBy.splice(ownedByIndex, 1);
                }
                break;
            }
        }
    }
    for (var i = 0; i < charAtlasCache.length; i++) {
        var entry = charAtlasCache[i];
        if (configEquals(entry.config, newConfig)) {
            entry.ownedBy.push(terminal);
            return entry.bitmap;
        }
    }
    var canvasFactory = function (width, height) {
        var canvas = document.createElement('canvas');
        canvas.width = width;
        canvas.height = height;
        return canvas;
    };
    var charAtlasConfig = {
        scaledCharWidth: scaledCharWidth,
        scaledCharHeight: scaledCharHeight,
        fontSize: terminal.options.fontSize,
        fontFamily: terminal.options.fontFamily,
        fontWeight: terminal.options.fontWeight,
        fontWeightBold: terminal.options.fontWeightBold,
        background: colors.background,
        foreground: colors.foreground,
        ansiColors: colors.ansi,
        devicePixelRatio: window.devicePixelRatio,
        allowTransparency: terminal.options.allowTransparency
    };
    var newEntry = {
        bitmap: CharAtlasGenerator_1.generateCharAtlas(window, canvasFactory, charAtlasConfig),
        config: newConfig,
        ownedBy: [terminal]
    };
    charAtlasCache.push(newEntry);
    return newEntry.bitmap;
}
exports.acquireCharAtlas = acquireCharAtlas;
function generateConfig(scaledCharWidth, scaledCharHeight, terminal, colors) {
    var clonedColors = {
        foreground: colors.foreground,
        background: colors.background,
        cursor: null,
        cursorAccent: null,
        selection: null,
        ansi: colors.ansi.slice(0, 16)
    };
    return {
        scaledCharWidth: scaledCharWidth,
        scaledCharHeight: scaledCharHeight,
        fontFamily: terminal.options.fontFamily,
        fontSize: terminal.options.fontSize,
        fontWeight: terminal.options.fontWeight,
        fontWeightBold: terminal.options.fontWeightBold,
        allowTransparency: terminal.options.allowTransparency,
        colors: clonedColors
    };
}
function configEquals(a, b) {
    for (var i = 0; i < a.colors.ansi.length; i++) {
        if (a.colors.ansi[i] !== b.colors.ansi[i]) {
            return false;
        }
    }
    return a.fontFamily === b.fontFamily &&
        a.fontSize === b.fontSize &&
        a.fontWeight === b.fontWeight &&
        a.fontWeightBold === b.fontWeightBold &&
        a.allowTransparency === b.allowTransparency &&
        a.scaledCharWidth === b.scaledCharWidth &&
        a.scaledCharHeight === b.scaledCharHeight &&
        a.colors.foreground === b.colors.foreground &&
        a.colors.background === b.colors.background;
}

//# sourceMappingURL=CharAtlas.js.map
