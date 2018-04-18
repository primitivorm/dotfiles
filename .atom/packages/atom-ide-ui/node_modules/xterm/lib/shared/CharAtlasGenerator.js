"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var Browser_1 = require("./utils/Browser");
exports.CHAR_ATLAS_CELL_SPACING = 1;
function generateCharAtlas(context, canvasFactory, request) {
    var cellWidth = request.scaledCharWidth + exports.CHAR_ATLAS_CELL_SPACING;
    var cellHeight = request.scaledCharHeight + exports.CHAR_ATLAS_CELL_SPACING;
    var canvas = canvasFactory(255 * cellWidth, (2 + 16) * cellHeight);
    var ctx = canvas.getContext('2d', { alpha: request.allowTransparency });
    ctx.fillStyle = request.background;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.save();
    ctx.fillStyle = request.foreground;
    ctx.font = getFont(request.fontWeight, request);
    ctx.textBaseline = 'top';
    for (var i = 0; i < 256; i++) {
        ctx.save();
        ctx.beginPath();
        ctx.rect(i * cellWidth, 0, cellWidth, cellHeight);
        ctx.clip();
        ctx.fillText(String.fromCharCode(i), i * cellWidth, 0);
        ctx.restore();
    }
    ctx.save();
    ctx.font = getFont(request.fontWeightBold, request);
    for (var i = 0; i < 256; i++) {
        ctx.save();
        ctx.beginPath();
        ctx.rect(i * cellWidth, cellHeight, cellWidth, cellHeight);
        ctx.clip();
        ctx.fillText(String.fromCharCode(i), i * cellWidth, cellHeight);
        ctx.restore();
    }
    ctx.restore();
    ctx.font = getFont(request.fontWeight, request);
    for (var colorIndex = 0; colorIndex < 16; colorIndex++) {
        if (colorIndex === 8) {
            ctx.font = getFont(request.fontWeightBold, request);
        }
        var y = (colorIndex + 2) * cellHeight;
        for (var i = 0; i < 256; i++) {
            ctx.save();
            ctx.beginPath();
            ctx.rect(i * cellWidth, y, cellWidth, cellHeight);
            ctx.clip();
            ctx.fillStyle = request.ansiColors[colorIndex];
            ctx.fillText(String.fromCharCode(i), i * cellWidth, y);
            ctx.restore();
        }
    }
    ctx.restore();
    if (!('createImageBitmap' in context) || Browser_1.isFirefox) {
        if (canvas instanceof HTMLCanvasElement) {
            return canvas;
        }
        else {
            return new Promise(function (r) { return r(canvas.transferToImageBitmap()); });
        }
    }
    var charAtlasImageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    var r = parseInt(request.background.substr(1, 2), 16);
    var g = parseInt(request.background.substr(3, 2), 16);
    var b = parseInt(request.background.substr(5, 2), 16);
    clearColor(charAtlasImageData, r, g, b);
    return context.createImageBitmap(charAtlasImageData);
}
exports.generateCharAtlas = generateCharAtlas;
function clearColor(imageData, r, g, b) {
    for (var offset = 0; offset < imageData.data.length; offset += 4) {
        if (imageData.data[offset] === r &&
            imageData.data[offset + 1] === g &&
            imageData.data[offset + 2] === b) {
            imageData.data[offset + 3] = 0;
        }
    }
}
function getFont(fontWeight, request) {
    return fontWeight + " " + request.fontSize * request.devicePixelRatio + "px " + request.fontFamily;
}

//# sourceMappingURL=CharAtlasGenerator.js.map
