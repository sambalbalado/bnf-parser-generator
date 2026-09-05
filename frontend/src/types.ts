export type State = Readonly<{
    grammar: string;
    string: string;
    selectedParser: string;
    run: boolean;
    resetParsers: boolean;
    grammarParseError: string;
    parsers: readonly string[];
    parserOutput: string;
    warnings: readonly string[];

    // New field for saving parser
    save: boolean;
}>;
