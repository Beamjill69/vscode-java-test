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

import com.microsoft.java.test.plugin.model.Option;
import com.microsoft.java.test.plugin.model.TestKind;
import com.microsoft.java.test.plugin.provider.TestKindProvider;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.AbstractTypeDeclaration;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.Block;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.IMethodBinding;
import org.eclipse.jdt.core.dom.IPackageBinding;
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
import org.eclipse.jdt.internal.core.manipulation.StubUtility;
import org.eclipse.jdt.internal.corext.codemanipulation.CodeGenerationSettings;
import org.eclipse.jdt.internal.corext.codemanipulation.ContextSensitiveImportRewriteContext;
import org.eclipse.jdt.internal.corext.codemanipulation.StubUtility2Core;
import org.eclipse.jdt.internal.corext.dom.ASTNodeFactory;
import org.eclipse.jdt.ls.core.internal.JDTUtils;
import org.eclipse.jdt.ls.core.internal.JavaLanguageServerPlugin;
import org.eclipse.jdt.ls.core.internal.handlers.CodeGenerationUtils;
import org.eclipse.jdt.ls.core.internal.text.correction.SourceAssistProcessor;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.text.edits.MalformedTreeException;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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

        final int cursorOffset = ((Double) arguments.get(1)).intValue();
        final NodeFinder nodeFinder = new NodeFinder(root, cursorOffset, 0);
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

        if (!binding.isClass()) {
            JavaLanguageServerPlugin.getInstance().getClientConnection().showNotificationMessage(MessageType.Error,
                    "Cannot generate tests if it's not a Java class.");
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
                    return generateTestsFromTest(unit, root, (TypeDeclaration) coveringNode, binding, cursorOffset);
                } else {
                    generateTestsFromSource();
                }
            }
        }

        return null;
    }

    private static void generateTestsFromSource() {
        // TODO: unimplement
    }

    private static WorkspaceEdit generateTestsFromTest(ICompilationUnit unit, CompilationUnit root,
            TypeDeclaration typeNode, ITypeBinding typeBinding, int cursorOffset)
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
                        (IType) typeBinding.getJavaElement(), cursorOffset);
        } catch (Throwable e) {
            // ignore if the upstream does not support insert position preference.
        }

        final TextEdit textEdit = createTextEditFromTestFile(testKind, methodsToGenerate, root, typeNode,
                typeBinding, insertPosition);
        return SourceAssistProcessor.convertToWorkspaceEdit(unit, textEdit);
    }

    private static List<String> determineMethodsToGenerate(List<String> lifecycleAnnotations) {
        final List<Option> methodList = lifecycleAnnotations.stream()
                .map(annotation -> new Option(annotation, "@" + annotation, 
                        capitalize(getSuggestedMethodNameByAnnotation(annotation)) + " Method"))
                .collect(Collectors.toList());
        methodList.add(0, new Option("Test", "@Test", "Test Method"));
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
            final List<Option> frameworkList = availableFrameworks.stream()
                .sorted((kind1, kind2) -> kind1.getValue() - kind2.getValue())
                .map(framework -> new Option(framework.toString()))
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

    private static TextEdit createTextEditFromTestFile(TestKind kind, List<String> methodsToGenerate,
            CompilationUnit root, TypeDeclaration typeNode, ITypeBinding typeBinding, IJavaElement insertPosition)
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
        final List<MethodMetaData> metadata = methodsToGenerate.stream().map(annotationName -> {
            final String methodName = getSuggestedMethodNameByAnnotation(annotationName);
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
        final Map<String, IMethodBinding> methodsMap = getMethodsBindings(typeBinding);
        for (final MethodMetaData method : methodMetadata) {
            final MethodDeclaration decl = ast.newMethodDeclaration();

            final boolean isStatic = needStaticModifier(kind, method.annotation);
            // set a unique method name according to the annotation type
            final String methodName = getUniqueMethodName(typeBinding.getJavaElement(), methodsMap,
                    method.methodName, isStatic);
            decl.setName(ast.newSimpleName(methodName));

            decl.modifiers().addAll(ASTNodeFactory.newModifiers(ast, getTestMethodModifiers(methodsMap, kind,
                    method.annotation, methodName)));
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

            if (needsOverrideAnnotation(isStatic, methodsMap.get(methodName), typeBinding)) {
                final CodeGenerationSettings settings = new CodeGenerationSettings();
                settings.overrideAnnotation = true;
                StubUtility2Core.addOverrideAnnotation(settings, root.getJavaElement().getJavaProject(), astRewrite,
                        importRewrite, decl, typeBinding.isInterface(), null);
            }

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

    private static List<String> getLifecycleAnnotations(TestKind testKind) {
        final List<String> list = new ArrayList<>();
        switch (testKind) {
            case JUnit:
                list.add(JUNIT4_BEFORE_CLASS_ANNOTATION);
                list.add(JUNIT4_SET_UP_ANNOTATION);
                list.add(JUNIT4_TEAR_DOWN_ANNOTATION);
                list.add(JUNIT4_AFTER_CLASS_ANNOTATION);
                break;
            case JUnit5:
                list.add(JUNIT5_BEFORE_CLASS_ANNOTATION);
                list.add(JUNIT5_SET_UP_ANNOTATION);
                list.add(JUNIT5_TEAR_DOWN_ANNOTATION);
                list.add(JUNIT5_AFTER_CLASS_ANNOTATION);
                break;
            case TestNG:
                list.add(TESTNG_BEFORE_CLASS_ANNOTATION);
                list.add(TESTNG_SET_UP_ANNOTATION);
                list.add(TESTNG_TEAR_DOWN_ANNOTATION);
                list.add(TESTNG_AFTER_CLASS_ANNOTATION);
                break;
            default:
                break;
        }
        return list;
    }

    private static String getSuggestedMethodNameByAnnotation(String annotation) {
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

    /**
     * return modifier bit mask.
     */
    private static int getTestMethodModifiers(Map<String, IMethodBinding> methodsMap, TestKind kind,
            String annotation, String methodName) {
        int modifiers = Modifier.NONE;

        // @BeforeClass and @AfterClass in JUnit 4 & 5 needs static modifier
        if (needStaticModifier(kind, annotation)) {
            modifiers |= Modifier.STATIC;
        }

        // JUnit 4's test method must be public
        if (kind == TestKind.JUnit) {
            modifiers |= Modifier.PUBLIC;
            return modifiers;
        }

        final IMethodBinding binding = methodsMap.get(methodName);
        if (binding == null) {
            return modifiers;
        }

        final int superModifiers = binding.getModifiers();
        if (Modifier.isProtected(superModifiers)) {
            modifiers |= Modifier.PROTECTED;
        } else if (Modifier.isPublic(superModifiers)) {
            modifiers |= Modifier.PUBLIC;
        }

        return modifiers;
    }

    private static Map<String, IMethodBinding> getMethodsBindings(ITypeBinding typeBinding) {
        final Map<String, IMethodBinding> methods = new HashMap<>();
        final IMethodBinding[] typeMethods = typeBinding.getDeclaredMethods();
        for (final IMethodBinding methodBinding : typeMethods) {
            methods.put(methodBinding.getName(), methodBinding);
        }
        ITypeBinding superClass = typeBinding.getSuperclass();
        while (superClass != null) {
            for (final IMethodBinding methodBinding : superClass.getDeclaredMethods()) {
                if (methods.containsKey(methodBinding.getName())) {
                    continue;
                }

                if (!isAccessible(methodBinding, typeBinding)) {
                    continue;
                }

                methods.put(methodBinding.getName(), methodBinding);
            }
            superClass = superClass.getSuperclass();
        }
        return methods;
    }

    private static boolean isAccessible(IMethodBinding superMethod, ITypeBinding declaredType) {
        final int modifiers = superMethod.getModifiers();
        if (Modifier.isPrivate(modifiers)) {
            return false;
        }

        if (!Modifier.isProtected(modifiers) && !Modifier.isPublic(modifiers)) {
            final IPackageBinding superMethodPackage = superMethod.getDeclaringClass().getPackage();
            final IPackageBinding declaredPackage = declaredType.getPackage();
            return superMethodPackage != null && declaredPackage != null &&
                    superMethodPackage.getName().equals(declaredPackage.getName());
        }

        return true;
    }

    private static boolean needStaticModifier(TestKind kind, String annotation) {
        if (annotation == null) {
            return false;
        }

        if (kind == TestKind.TestNG) {
            return false;
        }

        annotation = annotation.substring(annotation.lastIndexOf(".") + 1);
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

    private static boolean needsOverrideAnnotation(boolean isStatic, IMethodBinding methodBinding,
            ITypeBinding declaredType) {
        if (isStatic) {
            return false;
        }

        if (methodBinding == null) {
            return false;
        }
        if (Objects.equals(declaredType.getBinaryName(), methodBinding.getDeclaringClass().getBinaryName())) {
            return false;
        }
        return true;
    }

    private static String getUniqueMethodName(IJavaElement type, Map<String, IMethodBinding> methodsMap,
            String suggestedName, boolean isStatic) throws JavaModelException {
        if (type instanceof IType) {
            final IMethod[] methods = ((IType) type).getMethods();

            int suggestedPostfix = 0;
            String resultName = suggestedName;
            while (suggestedPostfix < 1000) {
                suggestedPostfix++;
                resultName = suggestedPostfix > 1 ? suggestedName + suggestedPostfix : suggestedName;
                if (hasMethod(methods, resultName)) {
                    continue;
                }
                final IMethodBinding superMethod = methodsMap.get(resultName);
                if (superMethod == null) {
                    return resultName;
                }
                if (!"void".equals(superMethod.getReturnType().getName())) {
                    continue;
                }
                final int modifier = superMethod.getModifiers();
                if (Modifier.isFinal(modifier)) {
                    continue;
                }
                if (Modifier.isStatic(modifier) != isStatic) {
                    continue;
                }
                return resultName;
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

    private static String capitalize(String str) {
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
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
