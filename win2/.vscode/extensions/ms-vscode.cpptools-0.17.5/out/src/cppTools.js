'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const LanguageServer = require("./LanguageServer/extension");
class CppTools {
    registerCustomConfigurationProvider(provider) {
        LanguageServer.registerCustomConfigurationProvider(provider);
    }
    didChangeCustomConfiguration(provider) {
        LanguageServer.onDidChangeCustomConfiguration(provider);
    }
}
exports.CppTools = CppTools;
//# sourceMappingURL=cppTools.js.map