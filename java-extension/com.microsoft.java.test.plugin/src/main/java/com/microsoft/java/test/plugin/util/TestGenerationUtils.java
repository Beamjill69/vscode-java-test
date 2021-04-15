/*******************************************************************************
* Copyright (c) 2021 Microsoft Corporation and others.
* All rights reserved. This program and the accompanying materials
* are made available under the terms of the Eclipse Public License v1.0
* which accompanies this distribution, and is available at
* http://www.eclipse.org/legal/epl-v10.html
*
* Contributors:
*     Microsoft Corporation - initial API and implementation
*******************************************************************************/

package com.microsoft.java.test.plugin.util;

import com.microsoft.java.test.plugin.model.TestKind;
import com.microsoft.java.test.plugin.provider.TestKindProvider;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.ITypeBinding;
import org.eclipse.jdt.core.dom.MethodDeclaration;
import org.eclipse.jdt.core.dom.Modifier;
import org.eclipse.jdt.core.dom.NodeFinder;
import org.eclipse.jdt.core.dom.PrimitiveType;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.dom.rewrite.ASTRewrite;
import org.eclipse.jdt.core.dom.rewrite.ImportRewrite;
import org.eclipse.jdt.core.dom.rewrite.ImportRewrite.ImportRewriteContext;
import org.eclipse.jdt.core.dom.rewrite.ListRewrite;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.refactoring.CompilationUnitChange;
import org.eclipse.jdt.internal.core.manipulation.StubUtility;
import org.eclipse.jdt.internal.corext.codemanipulation.ContextSensitiveImportRewriteContext;
import org.eclipse.jdt.internal.corext.codemanipulation.StubUtility2Core;
import org.eclipse.jdt.internal.corext.dom.ASTNodeFactory;
import org.eclipse.jdt.internal.corext.refactoring.changes.CreateCompilationUnitChange;
import org.eclipse.jdt.internal.corext.util.CodeFormatterUtil;
import org.eclipse.jdt.ls.core.internal.ChangeUtil;
import org.eclipse.jdt.ls.core.internal.JDTUtils;
import org.eclipse.jdt.ls.core.internal.JavaLanguageServerPlugin;
import org.eclipse.jdt.ls.core.internal.handlers.CodeGenerationUtils;
import org.eclipse.jdt.ls.core.internal.text.correction.SourceAssistProcessor;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class TestGenerationUtils {

    private static final String JUNIT4_PREFIX = "org.junit";
    private static final String JUNIT5_PREFIX = "org.junit.jupiter";
    private static final String TESTNG_PREFIX = "org.testng";

    private static final String JUNIT4_LIFECYCLE_ANNOTATION_PREFIX = "org.junit.";
    private static final String JUNIT4_BEFORE_CLASS_ANNOTATION = "BeforeClass";
    private static final String JUNIT4_SET_UP_ANNOTATION = "Before";
    private static final String JUNIT4_TEAR_DOWN_ANNOTATION = "After";
    private static final String JUNIT4_AFTER_CLASS_ANNOTATION = "AfterClass";

    private static final String JUNIT5_LIFECYCLE_ANNOTATION_PREFIX = "org.junit.jupiter.api.";
    private static final String JUNIT5_BEFORE_CLASS_ANNOTATION = "BeforeAll";
    private static final String JUNIT5_SET_UP_ANNOTATION = "BeforeEach";
    private static final String JUNIT5_TEAR_DOWN_ANNOTATION = "AfterEach";
    private static final String JUNIT5_AFTER_CLASS_ANNOTATION = "AfterAll";

    private static final String TESTNG_LIFECYCLE_ANNOTATION_PREFIX = "org.testng.annotations.";
    private static final String TESTNG_BEFORE_CLASS_ANNOTATION = "BeforeClass";
    private static final String TESTNG_SET_UP_ANNOTATION = "BeforeMethod";
    private static final String TESTNG_TEAR_DOWN_ANNOTATION = "AfterMethod";
    private static final String TESTNG_AFTER_CLASS_ANNOTATION = "AfterClass";

    public static WorkspaceEdit generateTests(List<Object> arguments, IProgressMonitor monitor)
            throws MalformedTreeException, CoreException {
        if (arguments == null || arguments.size() < 2) {
            throw new IllegalArgumentException("Wrong arguments passed to generate tests");
        }

        final String uri = (String) arguments.get(0);
        final ICompilationUnit unit = JDTUtils.resolveCompilationUnit(uri);
        if (unit == null) {
            JUnitPlugin.logError("Failed to parse compilation unit from " + uri);
            return null;
        }

        final CompilationUnit root = (CompilationUnit) TestSearchUtils.parseToAst(unit,
                true /* fromCache */, monitor);

        final int cursorPosition = ((Double) arguments.get(1)).intValue();
        final NodeFinder nodeFinder = new NodeFinder(root, cursorPosition, 0);
        ASTNode coveringNode = nodeFinder.getCoveringNode();

        while (coveringNode != null) {
            if (coveringNode instanceof TypeDeclaration) {
                break;
            }
            coveringNode = coveringNode.getParent();
        }
        if (coveringNode == null) {
            final IType primaryType = unit.findPrimaryType();
            if (primaryType == null) {
                return null;
            }
            coveringNode = root.findDeclaringNode(primaryType.getKey());
        }

        if (!(coveringNode instanceof TypeDeclaration)) {
            JUnitPlugin.logError("Failed to find type declaration from " + unit.getElementName());
            return null;
        }

        final ITypeBinding binding = ((TypeDeclaration) coveringNode).resolveBinding();
        if (binding == null) {
            JUnitPlugin.logError("Failed to resolve type binding from " + unit.getElementName());
            return null;
        }

        final IJavaProject javaProject = unit.getJavaProject();
        if (javaProject == null) {
            JUnitPlugin.logError("Cannot get Java project from " + unit.getElementName());
            return null;
        }

        for (final IClasspathEntry entry : javaProject.readRawClasspath()) {
            if (entry.getPath().isPrefixOf(unit.getPath())) {
                if (ProjectTestUtils.isTestEntry(entry)) {
                    return generateTestsFromTest(unit, root, (TypeDeclaration) coveringNode, binding, cursorPosition);
                } else {
                    return generateTestsFromSource(unit, binding, cursorPosition);
                }
            }
        }

        return null;
    }

    private static WorkspaceEdit generateTestsFromSource(ICompilationUnit unit, ITypeBinding typeBinding,
            int cursorPosition) throws CoreException {
        final List<TestKind> testFrameworksInProject = TestKindProvider.getTestKindsFromCache(unit.getJavaProject());
        final TestKind testKind = determineTestFramework(new HashSet<>(testFrameworksInProject));
        if (testKind == null) {
            return null;
        }

        final IClasspathEntry testEntry = getTestClasspathEntry(unit);
        final String testFullyQualifiedName = getTestFullyQualifiedName(typeBinding);
        if (testFullyQualifiedName == null) {
            return null;
        }
        final List<String> methodsToTest = getMethodsToTest(typeBinding);
        if (methodsToTest == null || methodsToTest.size() == 0) {
            return null;
        }

        final IPackageFragmentRoot packageRoot = unit.getJavaProject().findPackageFragmentRoot(testEntry.getPath());
        final String packageQualifiedName = testFullyQualifiedName.substring(0,
                testFullyQualifiedName.lastIndexOf("."));
        final IPackageFragment packageFragment = packageRoot.getPackageFragment(packageQualifiedName);
        final String compilationUnitName = testFullyQualifiedName.substring(
                testFullyQualifiedName.lastIndexOf(".") + 1) + ".java";
        final ICompilationUnit testUnit = packageFragment.getCompilationUnit(compilationUnitName);
        return createTextEditFromSourceFile(testKind, testUnit, methodsToTest, cursorPosition);
    }

    private static WorkspaceEdit generateTestsFromTest(ICompilationUnit unit, CompilationUnit root,
            TypeDeclaration typeNode, ITypeBinding typeBinding, int cursorPosition)
            throws MalformedTreeException, CoreException {
        final Set<TestKind> availableFrameworks = getTestKindFromFile(unit);
        if (availableFrameworks.size() == 0) {
            availableFrameworks.addAll(TestKindProvider.getTestKindsFromCache(unit.getJavaProject()));
        }

        final TestKind testKind = determineTestFramework(availableFrameworks);
        if (testKind == null) {
            return null;
        }

        final List<String> lifecycleAnnotations = getLifecycleAnnotations(testKind);
        final List<String> methodsToGenerate = determineMethodsToGenerate(lifecycleAnnotations);

        if (methodsToGenerate == null || methodsToGenerate.size() == 0) {
            return null;
        }

        IJavaElement insertPosition = null; // Insert to the last by default.
        try {
            insertPosition = CodeGenerationUtils.findInsertElement(
                        (IType) typeBinding.getJavaElement(), cursorPosition);
        } catch (Throwable e) {
            // ignore if the upstream does not support insert position preference.
        }

        final TextEdit textEdit = createTextEdit(testKind, methodsToGenerate, root, typeNode,
                typeBinding, insertPosition);
        return SourceAssistProcessor.convertToWorkspaceEdit(unit, textEdit);
    }

    private static List<String> determineMethodsToGenerate(List<String> lifecycleAnnotations) {
        final List<String> methodList = lifecycleAnnotations.stream()
                .map(annotation -> "@" + annotation.substring(annotation.lastIndexOf(".") + 1) + " Method")
                .collect(Collectors.toList());
        methodList.add(0, "@Test Method");
        return (List<String>) JUnitPlugin.askClientForChoice("Select methods to generate",
                methodList, true /*pickMany*/);
    }

    private static TestKind determineTestFramework(Set<TestKind> availableFrameworks) throws CoreException {
        if (availableFrameworks.size() == 0) {
            JavaLanguageServerPlugin.getInstance().getClientConnection().showNotificationMessage(MessageType.Error,
                    "Cannot find a unit test framework in the project, please make sure it's on the classpath.");
            return null;
        }
        if (availableFrameworks.size() == 1) {
            return availableFrameworks.iterator().next();
        } else {
            final List<String> frameworkList = availableFrameworks.stream()
                .sorted((kind1, kind2) -> kind1.getValueForRank() - kind2.getValueForRank())
                .map(framework -> framework.toString())
                .collect(Collectors.toList());

            final Object result = JUnitPlugin.askClientForChoice("Select a test framework to use", frameworkList);
            if (result == null) {
                return null;
            }
            return TestKind.fromString(((String) result));
        }
    }

    private static Set<TestKind> getTestKindFromFile(ICompilationUnit unit) throws JavaModelException {
        final IImportDeclaration[] imports = unit.getImports();
        final Set<TestKind> testKindsInFile = new HashSet<>();
        for (final IImportDeclaration importDeclaration : imports) {
            final String importPackage = importDeclaration.getElementName();
            if (importPackage.startsWith(JUNIT5_PREFIX)) {
                testKindsInFile.add(TestKind.JUnit5);
            } else if (importPackage.startsWith(JUNIT4_PREFIX)) {
                testKindsInFile.add(TestKind.JUnit);
            } else if (importPackage.startsWith(TESTNG_PREFIX)) {
                testKindsInFile.add(TestKind.TestNG);
            }
        }
        return testKindsInFile;
    }

    private static TextEdit createTextEdit(TestKind kind, List<String> methodsToGenerate, CompilationUnit root,
            TypeDeclaration typeNode, ITypeBinding typeBinding, IJavaElement insertPosition)
            throws MalformedTreeException, CoreException {
        String annotationPrefix = "";
        if (kind == TestKind.JUnit) {
            annotationPrefix = JUNIT4_LIFECYCLE_ANNOTATION_PREFIX;
        } else if (kind == TestKind.JUnit5) {
            annotationPrefix = JUNIT5_LIFECYCLE_ANNOTATION_PREFIX;
        } else if (kind == TestKind.TestNG) {
            annotationPrefix = TESTNG_LIFECYCLE_ANNOTATION_PREFIX;
        }

        final String prefix = annotationPrefix;
        final List<MethodMetaData> metadata = methodsToGenerate.stream().map(method -> {
            final String annotationName = method.substring(1 /*1 for leading "@"*/, method.lastIndexOf(" "));
            final String methodName = getPromptNameByAnnotation(annotationName);
            return new MethodMetaData(methodName, prefix + annotationName);
        }).collect(Collectors.toList());

        return getTextEdit(kind, metadata, root, typeNode, typeBinding, insertPosition);
    }

    private static TextEdit getTextEdit(TestKind kind, List<MethodMetaData> methodMetadata, CompilationUnit root,
            TypeDeclaration typeNode, ITypeBinding typeBinding, IJavaElement insertPosition)
            throws JavaModelException, CoreException {
        final ASTRewrite astRewrite = ASTRewrite.create(root.getAST());
        final ImportRewrite importRewrite = StubUtility.createImportRewrite(root, true);
        final ListRewrite listRewrite = astRewrite.getListRewrite(typeNode,
                ((AbstractTypeDeclaration) typeNode).getBodyDeclarationsProperty());
        final AST ast = astRewrite.getAST();
        for (final MethodMetaData method : methodMetadata) {
            
            final MethodDeclaration decl = ast.newMethodDeclaration();
            // JUnit 4's test method must be public
            if (kind == TestKind.JUnit) {
                decl.modifiers().addAll(ASTNodeFactory.newModifiers(ast, Modifier.PUBLIC));
            }

            // @BeforeClass and @AfterClass in JUnit 4 & 5 needs static modifier
            if (needStaticModifier(kind, method.annotation)) {
                decl.modifiers().addAll(ASTNodeFactory.newModifiers(ast, Modifier.STATIC));
            }

            // set a unique method name according to the annotation type
            decl.setName(ast.newSimpleName(getUniqueMethodName(typeBinding.getJavaElement(),
                    method.methodName)));
            decl.setConstructor(false);
            decl.setReturnType2(ast.newPrimitiveType(PrimitiveType.VOID));

            final Block body = ast.newBlock();
            // add a empty line in the method body
            body.statements().add(astRewrite.createStringPlaceholder("", ASTNode.EMPTY_STATEMENT));
            decl.setBody(body);

            // add the annotation and update the imports
            final Annotation marker = ast.newMarkerAnnotation();
            final ImportRewriteContext context = new ContextSensitiveImportRewriteContext(root,
                    decl.getStartPosition(), importRewrite);
            marker.setTypeName(ast.newName(importRewrite.addImport(method.annotation, context)));
            astRewrite.getListRewrite(decl, MethodDeclaration.MODIFIERS2_PROPERTY).insertFirst(marker, null);

            final ASTNode insertion = StubUtility2Core.getNodeToInsertBefore(listRewrite, insertPosition);
            if (insertion != null) {
                listRewrite.insertBefore(decl, insertion, null);
            } else {
                listRewrite.insertLast(decl, null);
            }
        }

        final MultiTextEdit edit = new MultiTextEdit();
        edit.addChild(importRewrite.rewriteImports(null));
        edit.addChild(astRewrite.rewriteAST());
        return edit;
    }

    private static IClasspathEntry getTestClasspathEntry(ICompilationUnit unit) throws JavaModelException {
        final IJavaProject javaProject = unit.getJavaProject();
        // In most cases, this is the classpath entry used for testing, we first find the target entry by hard-code
        // to avoid go into the generated entries.
        final IClasspathEntry testEntry = javaProject.getClasspathEntryFor(
            javaProject.getPath().append("src/test/java"));
        if (ProjectTestUtils.isTestEntry(testEntry)) {
            return testEntry;
        }

        final IClasspathEntry[] entries = javaProject.readRawClasspath();
        for (final IClasspathEntry entry : entries) {
            if (ProjectTestUtils.isTestEntry(entry)) {
                return entry;
            }
        }

        return entries[0];
    }

    private static String getTestFullyQualifiedName(ITypeBinding typeBinding) throws JavaModelException {
        // TODO: Check the existance of the case *Test/*Tests
        final String fullyQualifiedName = (String) JUnitPlugin.askClientForInput(
            "Please type the target test class name", typeBinding.getBinaryName() + "Tests");
        if (fullyQualifiedName == null) {
            return null;
        }

        if (fullyQualifiedName.charAt(fullyQualifiedName.length() - 1) == '.') {
            JavaLanguageServerPlugin.getInstance().getClientConnection().showNotificationMessage(MessageType.Error,
                    "Invalid Java class name: " + fullyQualifiedName);
            return null;
        }

        final String[] identifiers = fullyQualifiedName.split("\\.");
        for (final String identifier : identifiers) {
            final char[] chars = identifier.toCharArray();
            if (!Character.isJavaIdentifierStart(chars[0])) {
                JavaLanguageServerPlugin.getInstance().getClientConnection().showNotificationMessage(MessageType.Error,
                    "Invalid Java identifier: " + identifier);
                return null;
            }
            for (int i = 1; i < chars.length; i++) {
                if (!Character.isJavaIdentifierPart(chars[i])) {
                    JavaLanguageServerPlugin.getInstance().getClientConnection().showNotificationMessage(
                        MessageType.Error, "Invalid Java identifier: " + identifier);
                    return null;
                }
            }
        }

        return fullyQualifiedName;
    }

    private static List<String> getMethodsToTest(ITypeBinding typeBinding) {
        final List<IMethodBinding> allMethods = new LinkedList<>();
        final IMethodBinding[] typeMethods = typeBinding.getDeclaredMethods();
        for (final IMethodBinding method : typeMethods) {
            final int modifiers = method.getModifiers();
            if (!method.isConstructor() && !Modifier.isPrivate(modifiers) && !method.isSynthetic()) {
                allMethods.add(method);
            }
        }

        ITypeBinding clazz = typeBinding.getSuperclass();
        while (clazz != null && !"java.lang.Object".equals(clazz.getBinaryName())) {
            for (final IMethodBinding method : clazz.getDeclaredMethods()) {
                final int modifiers = method.getModifiers();
                if (!method.isConstructor() && !Modifier.isPrivate(modifiers) && !method.isSynthetic()) {
                    allMethods.add(method);
                }
            }
            clazz = clazz.getSuperclass();
        }

        final Set<String> methodNames = allMethods.stream()
            .map(method -> {
                final String returnValue = method.getReturnType().getName();
                final ITypeBinding[] paramTypes = method.getParameterTypes();
                final String params = String.join(", ",
                        Arrays.stream(paramTypes).map(t -> t.getName()).toArray(String[]::new));
                
                return returnValue + " " + method.getName() + "(" + params + ")";
            })
            .collect(Collectors.toSet());

        return (List<String>) JUnitPlugin.askClientForChoice("Select the methods to test",
                new ArrayList<>(methodNames), true /*pickMany*/);
    }

    private static WorkspaceEdit createTextEditFromSourceFile(TestKind kind, ICompilationUnit testUnit,
            List<String> methodsToTest, int cursorPosition) throws CoreException {
        if (testUnit.exists()) {
            final IType[] types = testUnit.getAllTypes();
            if (types.length == 0) {
                // an empty java file
                final CompilationUnitChange cuChange = new CompilationUnitChange("", testUnit);
                final String cuContent = constructNewCU(testUnit, methodsToTest, kind);
                cuChange.setEdit(new InsertEdit(0, cuContent));
                return ChangeUtil.convertToWorkspaceEdit(cuChange);
            }
            final CompilationUnit root = (CompilationUnit) TestSearchUtils.parseToAst(testUnit,
                false /* fromCache */, new NullProgressMonitor());

            if (root == null) {
                return null;
            }

            IType type = testUnit.findPrimaryType();
            if (type == null) {
                type = types[0];
            }

            final ASTNode typeNode = root.findDeclaringNode(type.getKey());
            if (!(typeNode instanceof TypeDeclaration)) {
                return null;
            }

            final ITypeBinding binding = ((TypeDeclaration) typeNode).resolveBinding();
            if (binding == null) {
                return null;
            }

            IJavaElement insertPosition = null; // Insert to the last by default.
            try {
                insertPosition = CodeGenerationUtils.findInsertElement(type, cursorPosition);
            } catch (Throwable e) {
                // ignore if the upstream does not support insert position preference.
            }

            return createTextEditFromSourceFile(root, kind, methodsToTest, (TypeDeclaration) typeNode,
                    binding, insertPosition);
        } else {
            final String cuContent = constructNewCU(testUnit, methodsToTest, kind);
            final CreateCompilationUnitChange change =
                    new CreateCompilationUnitChange(testUnit, cuContent, "");
            return ChangeUtil.convertToWorkspaceEdit(change);
        }
    }

    private static WorkspaceEdit createTextEditFromSourceFile(CompilationUnit testRoot, TestKind kind,
            List<String> methodsToTest, TypeDeclaration typeNode, ITypeBinding typeBinding,
            IJavaElement insertPosition) throws JavaModelException, CoreException {
        final String testAnnotation = getTestAnnotation(kind);
        final List<MethodMetaData> metadata = methodsToTest.stream().map(method -> {
            final String methodName = getTestMethodName(method);
            return new MethodMetaData(methodName, testAnnotation);
        }).collect(Collectors.toList());

        final TextEdit edit = getTextEdit(kind, metadata, testRoot, typeNode, typeBinding, insertPosition);
        return SourceAssistProcessor.convertToWorkspaceEdit((ICompilationUnit) testRoot.getJavaElement(), edit);
    }

    private static String constructNewCU(ICompilationUnit testUnit,
            List<String> methods, TestKind testKind) throws CoreException {
        final String delimiter = StubUtility.getLineDelimiterUsed(testUnit);
        final String typeStub = constructTypeStub(testUnit, methods, testKind, delimiter);
        final String cuContent = constructCUContent(testUnit, testKind, typeStub, delimiter);
        final String formattedCuStub = CodeFormatterUtil.format(
                CodeFormatter.K_COMPILATION_UNIT, cuContent, 0, delimiter, testUnit.getJavaProject().getOptions(true));
        return formattedCuStub;
    }

    private static String constructCUContent(ICompilationUnit testUnit, TestKind testKind, 
            String typeContent, String delimiter) throws CoreException {
        final IPackageFragment packageFragment = (IPackageFragment) testUnit.getParent();
        final StringBuilder buf = new StringBuilder();
        if (!packageFragment.isDefaultPackage()) {
            buf.append("package ")
                .append(packageFragment.getElementName())
                .append(";")
                .append(delimiter)
                .append(delimiter)
                .append("import ")
                .append(getTestAnnotation(testKind))
                .append(";")
                .append(delimiter)
                .append(delimiter);
        }
        buf.append(typeContent);
        return buf.toString();
    }

    private static String constructTypeStub(ICompilationUnit testUnit, List<String> methods,
            TestKind testKind, String delimiter) throws CoreException {
        final String typeName = testUnit.getElementName().replace(".java", "");
        final StringBuilder buf = new StringBuilder();
        buf.append("public class ").append(typeName).append(" {").append(delimiter);
        for (final String method : methods) {
            buf.append(constructMethodStub(testUnit, testKind, method, delimiter)).append(delimiter);
        }
        buf.append("}").append(delimiter);
        return buf.toString();
    }

    private static String constructMethodStub(ICompilationUnit testUnit, TestKind testKind,
            String method, String delimiter) {
        final StringBuilder buf = new StringBuilder();
        buf.append("@Test").append(delimiter);
        if (testKind == TestKind.JUnit) {
            buf.append("public ");
        }
        final String methodName = getTestMethodName(method);
        buf.append("void ").append(methodName).append("() {").append(delimiter).append(delimiter).append("}");
        final String methodContent = buf.toString();
        return CodeFormatterUtil.format(CodeFormatter.K_STATEMENTS, methodContent, 1,
                delimiter, testUnit.getJavaProject().getOptions(true));
    }

    private static String getTestMethodName(String focalMethodName) {
        final String methodName = focalMethodName.substring(focalMethodName.indexOf(" ") + 1,
                focalMethodName.indexOf("("));
        return "test" + Character.toUpperCase(methodName.charAt(0)) + methodName.substring(1);
    }

    private static List<String> getLifecycleAnnotations(TestKind testKind) {
        final List<String> list = new ArrayList<>();
        switch (testKind) {
            case JUnit:
                list.add(JUNIT4_LIFECYCLE_ANNOTATION_PREFIX + JUNIT4_BEFORE_CLASS_ANNOTATION);
                list.add(JUNIT4_LIFECYCLE_ANNOTATION_PREFIX + JUNIT4_SET_UP_ANNOTATION);
                list.add(JUNIT4_LIFECYCLE_ANNOTATION_PREFIX + JUNIT4_TEAR_DOWN_ANNOTATION);
                list.add(JUNIT4_LIFECYCLE_ANNOTATION_PREFIX + JUNIT4_AFTER_CLASS_ANNOTATION);
                break;
            case JUnit5:
                list.add(JUNIT5_LIFECYCLE_ANNOTATION_PREFIX + JUNIT5_BEFORE_CLASS_ANNOTATION);
                list.add(JUNIT5_LIFECYCLE_ANNOTATION_PREFIX + JUNIT5_SET_UP_ANNOTATION);
                list.add(JUNIT5_LIFECYCLE_ANNOTATION_PREFIX + JUNIT5_TEAR_DOWN_ANNOTATION);
                list.add(JUNIT5_LIFECYCLE_ANNOTATION_PREFIX + JUNIT5_AFTER_CLASS_ANNOTATION);
                break;
            case TestNG:
                list.add(TESTNG_LIFECYCLE_ANNOTATION_PREFIX + TESTNG_BEFORE_CLASS_ANNOTATION);
                list.add(TESTNG_LIFECYCLE_ANNOTATION_PREFIX + TESTNG_SET_UP_ANNOTATION);
                list.add(TESTNG_LIFECYCLE_ANNOTATION_PREFIX + TESTNG_TEAR_DOWN_ANNOTATION);
                list.add(TESTNG_LIFECYCLE_ANNOTATION_PREFIX + TESTNG_AFTER_CLASS_ANNOTATION);
                break;
            default:
                break;
        }
        return list;
    }

    private static String getPromptNameByAnnotation(String annotation) {
        switch (annotation) {
            case JUNIT4_BEFORE_CLASS_ANNOTATION:
            case JUNIT5_BEFORE_CLASS_ANNOTATION:
            // TestNG has the same annotation name with JUnit 4
            // case TESTNG_BEFORE_CLASS_ANNOTATION:
                return "beforeClass";
            case JUNIT4_AFTER_CLASS_ANNOTATION:
            case JUNIT5_AFTER_CLASS_ANNOTATION:
            // TestNG has the same annotation name with JUnit 4
            // case TESTNG_AFTER_CLASS_ANNOTATION:
                return "afterClass";
            case JUNIT4_SET_UP_ANNOTATION:
            case JUNIT5_SET_UP_ANNOTATION:
            case TESTNG_SET_UP_ANNOTATION:
                return "setUp";
            case JUNIT4_TEAR_DOWN_ANNOTATION:
            case JUNIT5_TEAR_DOWN_ANNOTATION:
            case TESTNG_TEAR_DOWN_ANNOTATION:
                return "tearDown";
            default:
                return "testName";
        }
    }

    private static boolean needStaticModifier(TestKind kind, String annotation) {
        if (annotation == null) {
            return false;
        }

        annotation = annotation.substring(annotation.lastIndexOf(".") + 1);


        if (kind == TestKind.TestNG) {
            return false;
        }

        if (kind == TestKind.JUnit) {
            switch (annotation) {
                case JUNIT4_BEFORE_CLASS_ANNOTATION:
                case JUNIT4_AFTER_CLASS_ANNOTATION:
                    return true;
                default:
                    return false;
            }
        }

        if (kind == TestKind.JUnit5) {
            switch (annotation) {
                case JUNIT5_BEFORE_CLASS_ANNOTATION:
                case JUNIT5_AFTER_CLASS_ANNOTATION:
                    return true;
                default:
                    return false;
            }
        }

        return false;
    }

    private static String getTestAnnotation(TestKind testKind) {
        if (testKind == TestKind.JUnit) {
            return "org.junit.Test";
        } else if (testKind == TestKind.TestNG) {
            return "org.testng.annotations.Test";
        }

        return "org.junit.jupiter.api.Test";
    }

    private static String getUniqueMethodName(IJavaElement type, String suggestedName) throws JavaModelException {
        if (type instanceof IType) {
            final IMethod[] methods = ((IType) type).getMethods();

            int suggestedPostfix = 2;
            String resultName = suggestedName;
            while (suggestedPostfix < 1000) {
                if (!hasMethod(methods, resultName)) {
                    return resultName;
                }
                resultName = suggestedName + suggestedPostfix++;
            }
        }

        return suggestedName;
    }

    private static boolean hasMethod(IMethod[] methods, String name) {
        for (final IMethod method : methods) {
            if (name.equals(method.getElementName())) {
                return true;
            }
        }
        return false;
    }

    static class MethodMetaData {
        public String methodName;
        public String annotation;

        public MethodMetaData(String methodName, String annotation) {
            this.methodName = methodName;
            this.annotation = annotation;
        }
    }
}
