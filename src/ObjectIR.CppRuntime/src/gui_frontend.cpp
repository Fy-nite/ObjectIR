#include "objectir_runtime.hpp"
#include "ir_loader.hpp"
#include "ir_text_parser.hpp"
#include "fob_loader.hpp"
#include <QApplication>
#include <QMainWindow>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QTextEdit>
#include <QPushButton>
#include <QLabel>
#include <QWidget>
#include <QFile>
#include <QTextStream>
#include <QFileDialog>
#include <QSplitter>
#include <QMenuBar>
#include <QMenu>
#include <QAction>
#include <QActionGroup>
#include <QStatusBar>
#include <QToolBar>
#include <QFont>
#include <QFontDatabase>
#include <QSyntaxHighlighter>
#include <QTextCharFormat>
#include <QRegularExpression>
#include <QGroupBox>
#include <QFileInfo>
#include <QMessageBox>
#include <iostream>
#include <string>
#include <functional>

using namespace ObjectIR;

// Theme enumeration
enum class Theme {
    Light,
    Dark,
    HighContrast,
    SolarizedLight,
    SolarizedDark
};

// Syntax highlighter for ObjectIR code
class ObjectIRHighlighter : public QSyntaxHighlighter {
    Q_OBJECT

public:
    ObjectIRHighlighter(QTextDocument *parent = nullptr) : QSyntaxHighlighter(parent), currentTheme(Theme::Dark) {
        setupColors();
        setupRules();
    }

    void setTheme(Theme theme) {
        currentTheme = theme;
        setupColors();
        rehighlight();
    }

private:
    void setupColors() {
        switch (currentTheme) {
            case Theme::Light:
                keywordFormat.setForeground(Qt::blue);
                instructionFormat.setForeground(Qt::darkGreen);
                typeFormat.setForeground(Qt::darkMagenta);
                stringFormat.setForeground(Qt::darkRed);
                commentFormat.setForeground(Qt::gray);
                break;

            case Theme::Dark:
                keywordFormat.setForeground(QColor(86, 156, 214));      // Light blue
                instructionFormat.setForeground(QColor(181, 206, 168)); // Light green
                typeFormat.setForeground(QColor(78, 201, 176));         // Teal
                stringFormat.setForeground(QColor(206, 145, 120));      // Orange
                commentFormat.setForeground(QColor(106, 153, 85));      // Green
                break;

            case Theme::HighContrast:
                keywordFormat.setForeground(QColor(255, 255, 0));       // Yellow
                instructionFormat.setForeground(QColor(0, 255, 0));      // Green
                typeFormat.setForeground(QColor(255, 0, 255));           // Magenta
                stringFormat.setForeground(QColor(0, 255, 255));         // Cyan
                commentFormat.setForeground(QColor(255, 255, 255));      // White
                break;

            case Theme::SolarizedLight:
                keywordFormat.setForeground(QColor(38, 139, 210));       // Blue
                instructionFormat.setForeground(QColor(42, 161, 152));   // Cyan
                typeFormat.setForeground(QColor(108, 113, 196));         // Violet
                stringFormat.setForeground(QColor(220, 50, 47));         // Red
                commentFormat.setForeground(QColor(147, 161, 161));      // Base1
                break;

            case Theme::SolarizedDark:
                keywordFormat.setForeground(QColor(38, 139, 210));       // Blue
                instructionFormat.setForeground(QColor(42, 161, 152));   // Cyan
                typeFormat.setForeground(QColor(108, 113, 196));         // Violet
                stringFormat.setForeground(QColor(220, 50, 47));         // Red
                commentFormat.setForeground(QColor(88, 110, 117));       // Base1
                break;
        }

        keywordFormat.setFontWeight(QFont::Bold);
        instructionFormat.setFontWeight(QFont::Bold);
    }

    void setupRules() {
        // Keywords
        QStringList keywords = {
            "module", "class", "interface", "struct", "method", "field",
            "public", "private", "protected", "static", "virtual", "abstract",
            "implements", "extends", "constructor", "destructor"
        };
        for (const QString &keyword : keywords) {
            rules.append({QRegularExpression("\\b" + keyword + "\\b"), keywordFormat});
        }

        // Instructions
        QStringList instructions = {
            "ldarg", "starg", "ldloc", "stloc", "ldstr", "ldc", "call", "callvirt",
            "ldfld", "stfld", "ldftn", "newobj", "newarr", "ret", "br", "brtrue",
            "brfalse", "beq", "bne", "blt", "ble", "bgt", "bge", "add", "sub",
            "mul", "div", "rem", "neg", "and", "or", "xor", "not", "shl", "shr",
            "conv", "castclass", "isinst", "dup", "pop", "nop"
        };
        for (const QString &instr : instructions) {
            rules.append({QRegularExpression("\\b" + instr + "\\b"), instructionFormat});
        }

        // Types
        QStringList types = {
            "void", "bool", "char", "int8", "int16", "int32", "int64",
            "uint8", "uint16", "uint32", "uint64", "float32", "float64",
            "string", "object"
        };
        for (const QString &type : types) {
            rules.append({QRegularExpression("\\b" + type + "\\b"), typeFormat});
        }

        // Strings
        rules.append({QRegularExpression("\".*\""), stringFormat});

        // Comments
        rules.append({QRegularExpression("//[^\n]*"), commentFormat});
    }

protected:
    void highlightBlock(const QString &text) override {
        for (const auto &rule : rules) {
            QRegularExpressionMatchIterator matchIterator = rule.pattern.globalMatch(text);
            while (matchIterator.hasNext()) {
                QRegularExpressionMatch match = matchIterator.next();
                setFormat(match.capturedStart(), match.capturedLength(), rule.format);
            }
        }
    }

private:
    struct HighlightingRule {
        QRegularExpression pattern;
        QTextCharFormat format;
    };
    QVector<HighlightingRule> rules;

    QTextCharFormat keywordFormat;
    QTextCharFormat instructionFormat;
    QTextCharFormat typeFormat;
    QTextCharFormat stringFormat;
    QTextCharFormat commentFormat;
    Theme currentTheme;
};

class MainWindow : public QMainWindow {
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr) : QMainWindow(parent) {
        setWindowTitle("ObjectIR Studio - Code, Compile, Execute");
        setGeometry(100, 100, 1200, 800);
        // Remove setWindowIcon for now to avoid potential issues

        // Initialize theme to Dark by default
        currentTheme = Theme::Dark;

        // Initialize basic window properties first
        setupCentralWidget();
        setupMenuBar();
        setupToolBar();
        setupStatusBar();

        // Apply initial theme
        applyTheme();

        updateStatusBar();
    }

private:
    void setupMenuBar() {
        // File menu
        QMenu *fileMenu = menuBar()->addMenu("&File");

        QAction *newAction = fileMenu->addAction("&New");
        newAction->setShortcut(QKeySequence::New);
        newAction->setStatusTip("Create a new ObjectIR program");
        connect(newAction, &QAction::triggered, this, &MainWindow::newFile);

        QAction *openAction = fileMenu->addAction("&Open...");
        openAction->setShortcut(QKeySequence::Open);
        openAction->setStatusTip("Open an ObjectIR file");
        connect(openAction, &QAction::triggered, this, &MainWindow::openFile);

        fileMenu->addSeparator();

        QAction *saveAction = fileMenu->addAction("&Save");
        saveAction->setShortcut(QKeySequence::Save);
        saveAction->setStatusTip("Save the current program");
        connect(saveAction, &QAction::triggered, this, &MainWindow::saveCode);

        QAction *saveAsAction = fileMenu->addAction("Save &As...");
        saveAsAction->setShortcut(QKeySequence::SaveAs);
        saveAsAction->setStatusTip("Save the current program with a new name");
        connect(saveAsAction, &QAction::triggered, this, &MainWindow::saveCodeAs);

        fileMenu->addSeparator();

        QAction *exitAction = fileMenu->addAction("E&xit");
        exitAction->setShortcut(QKeySequence::Quit);
        exitAction->setStatusTip("Exit ObjectIR Studio");
        connect(exitAction, &QAction::triggered, this, &QWidget::close);

        // Edit menu
        QMenu *editMenu = menuBar()->addMenu("&Edit");

        QAction *clearAction = editMenu->addAction("&Clear Output");
        clearAction->setShortcut(QKeySequence("Ctrl+L"));
        clearAction->setStatusTip("Clear the output window");
        connect(clearAction, &QAction::triggered, outputEdit, &QTextEdit::clear);

        // Run menu
        QMenu *runMenu = menuBar()->addMenu("&Run");

        QAction *runAction = runMenu->addAction("&Run");
        runAction->setShortcut(QKeySequence("F5"));
        runAction->setStatusTip("Execute the current program");
        connect(runAction, &QAction::triggered, this, &MainWindow::runCode);

        // Help menu
        QMenu *helpMenu = menuBar()->addMenu("&Help");

        QAction *aboutAction = helpMenu->addAction("&About");
        aboutAction->setStatusTip("About ObjectIR Studio");
        connect(aboutAction, &QAction::triggered, this, &MainWindow::showAbout);

        // View menu
        QMenu *viewMenu = menuBar()->addMenu("&View");

        QMenu *themeMenu = viewMenu->addMenu("&Theme");

        QAction *lightThemeAction = themeMenu->addAction("&Light");
        lightThemeAction->setCheckable(true);
        lightThemeAction->setChecked(currentTheme == Theme::Light);
        connect(lightThemeAction, &QAction::triggered, [this]() { setTheme(Theme::Light); });

        QAction *darkThemeAction = themeMenu->addAction("&Dark");
        darkThemeAction->setCheckable(true);
        darkThemeAction->setChecked(currentTheme == Theme::Dark);
        connect(darkThemeAction, &QAction::triggered, [this]() { setTheme(Theme::Dark); });

        QAction *highContrastThemeAction = themeMenu->addAction("&High Contrast");
        highContrastThemeAction->setCheckable(true);
        highContrastThemeAction->setChecked(currentTheme == Theme::HighContrast);
        connect(highContrastThemeAction, &QAction::triggered, [this]() { setTheme(Theme::HighContrast); });

        QAction *solarizedLightThemeAction = themeMenu->addAction("Solarized &Light");
        solarizedLightThemeAction->setCheckable(true);
        solarizedLightThemeAction->setChecked(currentTheme == Theme::SolarizedLight);
        connect(solarizedLightThemeAction, &QAction::triggered, [this]() { setTheme(Theme::SolarizedLight); });

        QAction *solarizedDarkThemeAction = themeMenu->addAction("Solarized &Dark");
        solarizedDarkThemeAction->setCheckable(true);
        solarizedDarkThemeAction->setChecked(currentTheme == Theme::SolarizedDark);
        connect(solarizedDarkThemeAction, &QAction::triggered, [this]() { setTheme(Theme::SolarizedDark); });

        // Make theme actions mutually exclusive
        QActionGroup *themeGroup = new QActionGroup(this);
        themeGroup->addAction(lightThemeAction);
        themeGroup->addAction(darkThemeAction);
        themeGroup->addAction(highContrastThemeAction);
        themeGroup->addAction(solarizedLightThemeAction);
        themeGroup->addAction(solarizedDarkThemeAction);
    }

    void setupToolBar() {
        QToolBar *toolBar = addToolBar("Main");
        toolBar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

        QAction *newAction = toolBar->addAction("New");
        newAction->setShortcut(QKeySequence::New);
        connect(newAction, &QAction::triggered, this, &MainWindow::newFile);

        toolBar->addSeparator();

        QAction *openAction = toolBar->addAction("Open");
        openAction->setShortcut(QKeySequence::Open);
        connect(openAction, &QAction::triggered, this, &MainWindow::openFile);

        QAction *saveAction = toolBar->addAction("Save");
        saveAction->setShortcut(QKeySequence::Save);
        connect(saveAction, &QAction::triggered, this, &MainWindow::saveCode);

        toolBar->addSeparator();

        QAction *runAction = toolBar->addAction("Run");
        runAction->setShortcut(QKeySequence("F5"));
        connect(runAction, &QAction::triggered, this, &MainWindow::runCode);

        QAction *clearAction = toolBar->addAction("Clear");
        connect(clearAction, &QAction::triggered, outputEdit, &QTextEdit::clear);
    }

    void setupCentralWidget() {
        QWidget *centralWidget = new QWidget;
        setCentralWidget(centralWidget);

        QVBoxLayout *mainLayout = new QVBoxLayout(centralWidget);
        mainLayout->setContentsMargins(10, 10, 10, 10);
        mainLayout->setSpacing(10);

        // Title
        QLabel *titleLabel = new QLabel("🎯 ObjectIR Studio");
        titleLabel->setStyleSheet("font-size: 18pt; color: #0078d4; margin-bottom: 10px;");
        mainLayout->addWidget(titleLabel);

        QLabel *subtitleLabel = new QLabel("Write, compile, and execute ObjectIR programs with ease");
        subtitleLabel->setStyleSheet("color: #666; margin-bottom: 15px;");
        mainLayout->addWidget(subtitleLabel);

        // Main splitter for code and output
        QSplitter *mainSplitter = new QSplitter(Qt::Vertical);
        mainSplitter->setChildrenCollapsible(false);

        // Code section
        QWidget *codeWidget = new QWidget;
        QVBoxLayout *codeLayout = new QVBoxLayout(codeWidget);
        codeLayout->setContentsMargins(0, 0, 0, 0);

        QGroupBox *codeGroup = new QGroupBox("📝 Source Code");
        QVBoxLayout *codeGroupLayout = new QVBoxLayout(codeGroup);

        codeEdit = new QTextEdit;
        codeEdit->setPlainText(R"(module HelloWorld

class Program {
    method Main() -> void {
        ldstr "Hello from ObjectIR!"
        call System.Console.WriteLine(string) -> void
        ret
    }
}
)");
        codeEdit->setMinimumHeight(300);

        // Add syntax highlighting
        highlighter = new ObjectIRHighlighter(codeEdit->document());

        codeGroupLayout->addWidget(codeEdit);
        codeLayout->addWidget(codeGroup);
        mainSplitter->addWidget(codeWidget);

        // Output section
        QWidget *outputWidget = new QWidget;
        QVBoxLayout *outputLayout = new QVBoxLayout(outputWidget);
        outputLayout->setContentsMargins(0, 0, 0, 0);

        QGroupBox *outputGroup = new QGroupBox("⚡ Output");
        QVBoxLayout *outputGroupLayout = new QVBoxLayout(outputGroup);

        outputEdit = new QTextEdit;
        outputEdit->setReadOnly(true);
        outputEdit->setMaximumHeight(200);
        // Background will be set by stylesheet

        outputGroupLayout->addWidget(outputEdit);
        outputLayout->addWidget(outputGroup);
        mainSplitter->addWidget(outputWidget);

        // Set splitter proportions
        mainSplitter->setStretchFactor(0, 3);
        mainSplitter->setStretchFactor(1, 1);

        mainLayout->addWidget(mainSplitter);
    }

    void setupStatusBar() {
        statusBar = new QStatusBar;
        setStatusBar(statusBar);

        statusLabel = new QLabel("Ready");
        statusBar->addWidget(statusLabel);

        statusBar->addPermanentWidget(new QLabel("ObjectIR Studio v1.0"));
    }

    void updateStatusBar() {
        QString status = "Ready";
        if (!currentFile.isEmpty()) {
            status = "File: " + QFileInfo(currentFile).fileName();
        }
        statusLabel->setText(status);
    }

    void applyTheme() {
        QString styleSheet;
        switch (currentTheme) {
            case Theme::Dark:
                styleSheet = R"(
                    QMainWindow {
                        background-color: #2b2b2b;
                        color: #ffffff;
                    }
                    QTextEdit {
                        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                        font-size: 10pt;
                        background-color: #1e1e1e;
                        color: #ffffff;
                        border: 1px solid #555;
                        border-radius: 3px;
                        padding: 5px;
                    }
                    QTextEdit[readOnly="true"] {
                        background-color: #252526;
                    }
                    QPushButton {
                        background-color: #0e639c;
                        color: white;
                        border: none;
                        padding: 8px 16px;
                        border-radius: 3px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #1177bb;
                    }
                    QPushButton:pressed {
                        background-color: #0d5a8a;
                    }
                    QLabel {
                        color: #ffffff;
                        font-weight: bold;
                    }
                    QGroupBox {
                        font-weight: bold;
                        border: 2px solid #555;
                        border-radius: 5px;
                        margin-top: 1ex;
                        color: #ffffff;
                    }
                    QGroupBox::title {
                        subcontrol-origin: margin;
                        left: 10px;
                        padding: 0 5px 0 5px;
                    }
                    QMenuBar {
                        background-color: #2b2b2b;
                        color: #ffffff;
                    }
                    QMenuBar::item {
                        background-color: transparent;
                        padding: 4px 8px;
                    }
                    QMenuBar::item:selected {
                        background-color: #3d3d3d;
                    }
                    QMenu {
                        background-color: #2b2b2b;
                        color: #ffffff;
                        border: 1px solid #555;
                    }
                    QMenu::item:selected {
                        background-color: #3d3d3d;
                    }
                    QStatusBar {
                        background-color: #2b2b2b;
                        color: #ffffff;
                        border-top: 1px solid #555;
                    }
                )";
                break;

            case Theme::Light:
                styleSheet = R"(
                    QMainWindow {
                        background-color: #f5f5f5;
                    }
                    QTextEdit {
                        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                        font-size: 10pt;
                        background-color: white;
                        border: 1px solid #ccc;
                        border-radius: 3px;
                        padding: 5px;
                    }
                    QTextEdit[readOnly="true"] {
                        background-color: #f8f8f8;
                    }
                    QPushButton {
                        background-color: #0078d4;
                        color: white;
                        border: none;
                        padding: 8px 16px;
                        border-radius: 3px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #106ebe;
                    }
                    QPushButton:pressed {
                        background-color: #005a9e;
                    }
                    QLabel {
                        color: #333;
                        font-weight: bold;
                    }
                    QGroupBox {
                        font-weight: bold;
                        border: 2px solid #ccc;
                        border-radius: 5px;
                        margin-top: 1ex;
                    }
                    QGroupBox::title {
                        subcontrol-origin: margin;
                        left: 10px;
                        padding: 0 5px 0 5px;
                    }
                    QMenuBar {
                        background-color: #f5f5f5;
                        color: #000000;
                    }
                    QMenuBar::item {
                        background-color: transparent;
                        padding: 4px 8px;
                    }
                    QMenuBar::item:selected {
                        background-color: #e0e0e0;
                    }
                    QMenu {
                        background-color: #ffffff;
                        color: #000000;
                        border: 1px solid #ccc;
                    }
                    QMenu::item:selected {
                        background-color: #e0e0e0;
                    }
                    QStatusBar {
                        background-color: #f5f5f5;
                        color: #000000;
                        border-top: 1px solid #ccc;
                    }
                )";
                break;

            case Theme::HighContrast:
                styleSheet = R"(
                    QMainWindow {
                        background-color: #000000;
                        color: #ffffff;
                    }
                    QTextEdit {
                        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                        font-size: 12pt;
                        font-weight: bold;
                        background-color: #000000;
                        color: #ffff00;
                        border: 2px solid #ffffff;
                        border-radius: 3px;
                        padding: 5px;
                    }
                    QTextEdit[readOnly="true"] {
                        background-color: #000000;
                        color: #00ff00;
                    }
                    QPushButton {
                        background-color: #ffffff;
                        color: #000000;
                        border: 2px solid #000000;
                        padding: 10px 20px;
                        border-radius: 3px;
                        font-weight: bold;
                        font-size: 11pt;
                    }
                    QPushButton:hover {
                        background-color: #ffff00;
                    }
                    QPushButton:pressed {
                        background-color: #ff0000;
                    }
                    QLabel {
                        color: #ffffff;
                        font-weight: bold;
                        font-size: 11pt;
                    }
                    QGroupBox {
                        font-weight: bold;
                        border: 3px solid #ffffff;
                        border-radius: 5px;
                        margin-top: 1ex;
                        color: #ffffff;
                        font-size: 11pt;
                    }
                    QGroupBox::title {
                        subcontrol-origin: margin;
                        left: 10px;
                        padding: 0 5px 0 5px;
                    }
                    QMenuBar {
                        background-color: #000000;
                        color: #ffffff;
                        border-bottom: 2px solid #ffffff;
                    }
                    QMenuBar::item {
                        background-color: transparent;
                        padding: 6px 10px;
                        font-size: 11pt;
                    }
                    QMenuBar::item:selected {
                        background-color: #ffffff;
                        color: #000000;
                    }
                    QMenu {
                        background-color: #000000;
                        color: #ffffff;
                        border: 2px solid #ffffff;
                        font-size: 11pt;
                    }
                    QMenu::item:selected {
                        background-color: #ffffff;
                        color: #000000;
                    }
                    QStatusBar {
                        background-color: #000000;
                        color: #ffffff;
                        border-top: 2px solid #ffffff;
                        font-size: 11pt;
                    }
                )";
                break;

            case Theme::SolarizedLight:
                styleSheet = R"(
                    QMainWindow {
                        background-color: #fdf6e3;
                        color: #586e75;
                    }
                    QTextEdit {
                        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                        font-size: 10pt;
                        background-color: #eee8d5;
                        color: #586e75;
                        border: 1px solid #93a1a1;
                        border-radius: 3px;
                        padding: 5px;
                    }
                    QTextEdit[readOnly="true"] {
                        background-color: #f5f1e8;
                    }
                    QPushButton {
                        background-color: #268bd2;
                        color: #fdf6e3;
                        border: none;
                        padding: 8px 16px;
                        border-radius: 3px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #2aa198;
                    }
                    QPushButton:pressed {
                        background-color: #dc322f;
                    }
                    QLabel {
                        color: #586e75;
                        font-weight: bold;
                    }
                    QGroupBox {
                        font-weight: bold;
                        border: 2px solid #93a1a1;
                        border-radius: 5px;
                        margin-top: 1ex;
                        color: #586e75;
                    }
                    QGroupBox::title {
                        subcontrol-origin: margin;
                        left: 10px;
                        padding: 0 5px 0 5px;
                    }
                    QMenuBar {
                        background-color: #fdf6e3;
                        color: #586e75;
                    }
                    QMenuBar::item {
                        background-color: transparent;
                        padding: 4px 8px;
                    }
                    QMenuBar::item:selected {
                        background-color: #eee8d5;
                    }
                    QMenu {
                        background-color: #fdf6e3;
                        color: #586e75;
                        border: 1px solid #93a1a1;
                    }
                    QMenu::item:selected {
                        background-color: #eee8d5;
                    }
                    QStatusBar {
                        background-color: #fdf6e3;
                        color: #586e75;
                        border-top: 1px solid #93a1a1;
                    }
                )";
                break;

            case Theme::SolarizedDark:
                styleSheet = R"(
                    QMainWindow {
                        background-color: #002b36;
                        color: #839496;
                    }
                    QTextEdit {
                        font-family: 'Consolas', 'Monaco', 'Courier New', monospace;
                        font-size: 10pt;
                        background-color: #073642;
                        color: #839496;
                        border: 1px solid #586e75;
                        border-radius: 3px;
                        padding: 5px;
                    }
                    QTextEdit[readOnly="true"] {
                        background-color: #0a3742;
                    }
                    QPushButton {
                        background-color: #268bd2;
                        color: #fdf6e3;
                        border: none;
                        padding: 8px 16px;
                        border-radius: 3px;
                        font-weight: bold;
                    }
                    QPushButton:hover {
                        background-color: #2aa198;
                    }
                    QPushButton:pressed {
                        background-color: #dc322f;
                    }
                    QLabel {
                        color: #839496;
                        font-weight: bold;
                    }
                    QGroupBox {
                        font-weight: bold;
                        border: 2px solid #586e75;
                        border-radius: 5px;
                        margin-top: 1ex;
                        color: #839496;
                    }
                    QGroupBox::title {
                        subcontrol-origin: margin;
                        left: 10px;
                        padding: 0 5px 0 5px;
                    }
                    QMenuBar {
                        background-color: #002b36;
                        color: #839496;
                    }
                    QMenuBar::item {
                        background-color: transparent;
                        padding: 4px 8px;
                    }
                    QMenuBar::item:selected {
                        background-color: #073642;
                    }
                    QMenu {
                        background-color: #002b36;
                        color: #839496;
                        border: 1px solid #586e75;
                    }
                    QMenu::item:selected {
                        background-color: #073642;
                    }
                    QStatusBar {
                        background-color: #002b36;
                        color: #839496;
                        border-top: 1px solid #586e75;
                    }
                )";
                break;
        }
        setStyleSheet(styleSheet);

        // Update highlighter theme
        if (highlighter) {
            highlighter->setTheme(currentTheme);
        }
    }

private slots:
    void newFile() {
        codeEdit->clear();
        outputEdit->clear();
        currentFile.clear();
        currentFOBFile.clear();
        updateStatusBar();
        setWindowTitle("ObjectIR Studio - Code, Compile, Execute");
    }

    void saveCodeAs() {
        saveCode();
    }

    void showAbout() {
        QMessageBox::about(this, "About ObjectIR Studio",
            "<h2>ObjectIR Studio</h2>"
            "<p>A modern IDE for ObjectIR programming</p>"
            "<p><b>Features:</b></p>"
            "<ul>"
            "<li>Syntax highlighting with multiple themes</li>"
            "<li>FOB bytecode compilation</li>"
            "<li>Direct execution</li>"
            "<li>Modern UI with theme support</li>"
            "<li>Multiple themes: Light, Dark, High Contrast, Solarized</li>"
            "</ul>"
            "<p><b>Version:</b> 1.0</p>");
    }

    void setTheme(Theme theme) {
        currentTheme = theme;
        applyTheme();
    }

    void appendToOutput(const std::string& text) {
        QString qtext = QString::fromStdString(text);
        if (qtext.endsWith('\n')) {
            qtext.chop(1);
        }
        if (!qtext.isEmpty()) {
            outputEdit->setPlainText(outputEdit->toPlainText() + qtext + "\n");
        }
    }

private slots:
    void openFile() {
        QString fileName = QFileDialog::getOpenFileName(this, 
            "Open ObjectIR File", "",
            "ObjectIR Text Files (*.ir *.ir.txt);;FOB Files (*.fob);;All Files (*)");
        
        if (!fileName.isEmpty()) {
            if (fileName.endsWith(".fob")) {
                // FOB files are binary - can't edit them in text editor
                outputEdit->setPlainText("FOB files can only be executed, not edited.\n"
                                       "Use 'Run' to execute: " + fileName);
                // Store the FOB file path for execution
                currentFOBFile = fileName;
            } else {
                // Load text IR file
                QFile file(fileName);
                if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
                    QTextStream in(&file);
                    codeEdit->setPlainText(in.readAll());
                    file.close();
                    outputEdit->setPlainText("Loaded text IR: " + fileName);
                    currentFile = fileName;
                    currentFOBFile.clear(); // Clear any stored FOB file
                    updateStatusBar();
                    setWindowTitle("ObjectIR Studio - " + QFileInfo(fileName).fileName());
                } else {
                    outputEdit->setPlainText("Failed to open file");
                }
            }
        }
    }

    void saveCode() {
        QString fileName = QFileDialog::getSaveFileName(this,
            "Save ObjectIR File", "",
            "FOB Files (*.fob);;ObjectIR Text Files (*.ir);;Text Files (*.ir.txt);;All Files (*)");
        
        if (!fileName.isEmpty()) {
            QString code = codeEdit->toPlainText();
            
            // Check if user wants FOB format
            if (fileName.endsWith(".fob")) {
                try {
                    // Parse to FOB bytecode
                    auto fobData = IRTextParser::ParseToFOB(code.toStdString());
                    
                    QFile file(fileName);
                    if (file.open(QIODevice::WriteOnly)) {
                        file.write(reinterpret_cast<const char*>(fobData.data()), fobData.size());
                        file.close();
                        outputEdit->setPlainText("Compiled and saved as FOB: " + fileName + 
                                               "\nSize: " + QString::number(fobData.size()) + " bytes");
                        currentFile = fileName;
                        updateStatusBar();
                        setWindowTitle("ObjectIR Studio - " + QFileInfo(fileName).fileName());
                    } else {
                        outputEdit->setPlainText("Failed to save FOB file");
                    }
                } catch (const std::exception& e) {
                    outputEdit->setPlainText("FOB compilation failed: " + QString::fromStdString(e.what()));
                }
            } else {
                // Save as text IR
                QFile file(fileName);
                if (file.open(QIODevice::WriteOnly | QIODevice::Text)) {
                    QTextStream out(&file);
                    out << code;
                    file.close();
                    outputEdit->setPlainText("Saved as text IR: " + fileName);
                    currentFile = fileName;
                    updateStatusBar();
                    setWindowTitle("ObjectIR Studio - " + QFileInfo(fileName).fileName());
                } else {
                    outputEdit->setPlainText("Failed to save file");
                }
            }
        }
    }

    void runCode() {
        outputEdit->clear();
        outputEdit->setPlainText("Executing...\n");
        
        try {
            std::cout << "\n=== Starting Execution ===" << std::endl;
            
            std::shared_ptr<VirtualMachine> vm;
            
            if (!currentFOBFile.isEmpty()) {
                // Execute FOB file
                outputEdit->append("Loading FOB file: " + currentFOBFile + "\n");
                auto fobResult = FOBLoader::LoadFromFile(currentFOBFile.toStdString());
                vm = fobResult.vm;
                outputEdit->append("FOB loaded successfully\n");
            } else {
                // Parse and execute text IR
                QString code = codeEdit->toPlainText();
                outputEdit->append("Parsing text IR...\n");
                vm = IRTextParser::ParseToVirtualMachine(code.toStdString());
                outputEdit->append("Text IR parsed successfully\n");
            }
            
            // Set up output redirection to GUI console IMMEDIATELY after VM creation
            vm->SetOutputFunction(std::bind(&MainWindow::appendToOutput, this, std::placeholders::_1));
            
            // Find and execute Main method
            auto mainClass = vm->GetClass("Program");
            if (!mainClass) {
                mainClass = vm->GetClass("Main");
            }
            
            if (mainClass) {
                std::cout << "Found class, looking for Main method..." << std::endl;
                std::vector<Value> args;
                vm->InvokeStaticMethod(mainClass, "Main", args);
                outputEdit->append("\n=== Execution Complete ===");
                std::cout << "Execution completed successfully" << std::endl;
            } else {
                outputEdit->setPlainText("Error: No Program or Main class found");
                std::cerr << "ERROR: Could not find Program or Main class" << std::endl;
            }
        } catch (const std::exception& e) {
            std::string errorMsg = e.what();
            outputEdit->setPlainText(QString("Error: ") + QString::fromStdString(errorMsg));
            std::cerr << "EXCEPTION: " << errorMsg << std::endl;
        }
    }

private:
    QTextEdit *codeEdit;
    QTextEdit *outputEdit;
    ObjectIRHighlighter *highlighter;
    QStatusBar *statusBar;
    QLabel *statusLabel;
    QString currentFile;
    QString currentFOBFile;
    Theme currentTheme;
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    std::cout << "Starting ObjectIR Studio..." << std::endl;
    MainWindow window;
    std::cout << "MainWindow created" << std::endl;
    window.show();
    std::cout << "MainWindow shown" << std::endl;
    return app.exec();
    std::cout << "Application exited" << std::endl;
}

#include "gui_frontend.moc"