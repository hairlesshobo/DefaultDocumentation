using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Xml.Linq;
using DefaultDocumentation.Helper;
using DefaultDocumentation.Model;
using DefaultDocumentation.Model.Member;
using DefaultDocumentation.Model.Parameter;
using DefaultDocumentation.Model.Type;
using ICSharpCode.Decompiler.Documentation;
using ICSharpCode.Decompiler.TypeSystem;
using ICSharpCode.Decompiler.TypeSystem.Implementation;

namespace DefaultDocumentation.Writer
{
    internal sealed class MsStyleMarkdownWriter : DocItemWriter
    {
        private readonly ConcurrentDictionary<string, string> _urls;
        private readonly StringBuilder _builder;
        private readonly Func<DocItem, string> _urlFactory;

        private bool _ignoreLineBreak;

        public MsStyleMarkdownWriter(Settings settings)
            : base(settings)
        {
            _urls = new ConcurrentDictionary<string, string>();
            _builder = new StringBuilder();
            _urlFactory = item =>
            {
                if (item is ExternDocItem externItem)
                {
                    return externItem.Url;
                }

                DocItem pagedItem = item;
                while (!HasOwnPage(pagedItem))
                {
                    pagedItem = pagedItem.Parent;
                }

                string url = GetFileName(pagedItem);
                if (!settings.RemoveFileExtensionFromLinks)
                {
                    url += ".md";
                }
                if (item != pagedItem)
                {
                    url += "#" + settings.PathCleaner.Clean(item.FullName);
                }

                return url;
            };

            _ignoreLineBreak = settings.IgnoreLineBreak;
        }

        private static string ToLink(string url, string displayedName = null) => $"[{(displayedName ?? url).Prettify()}]({url} '{url}')";

        private string GetLink(DocItem item, string displayedName = null) => $"[{displayedName ?? item.Name}]({GetUrl(item)} '{item.FullName}')";

        private string GetLink(string id, string displayedName = null) => TryGetDocItem(id, out DocItem item) ? GetLink(item, displayedName) : $"[{(displayedName ?? id.Substring(2)).Prettify()}]({_urls.GetOrAdd(id, i => i.ToDotNetApiUrl())} '{id.Substring(2)}')";

        private string GetLink(DocItem item, INamedElement element)
        {
            string HandleParameterizedType(ParameterizedType genericType)
            {
                string id = genericType.GetDefinition().GetIdString();

                return GetLink(id, genericType.FullName + "&lt;")
                    + string.Join(GetLink(id, ","), genericType.TypeArguments.Select(t => GetLink(item, t)))
                    + GetLink(id, "&gt;");
            }

            string HandleFunctionPointer(FunctionPointerType functionPointerType)
            {
                const string reference = "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-9.0/function-pointers";

                return ToLink(reference, "delegate*<")
                    + string.Join(ToLink(reference, ","), functionPointerType.ParameterTypes.Concat(Enumerable.Repeat(functionPointerType.ReturnType, 1)).Select(t => GetLink(item, t)))
                    + ToLink(reference, ">");
            }

            string HandleTupleType(TupleType tupleType)
            {
                return GetLink("T:" + tupleType.FullName, "&lt;")
                    + string.Join(GetLink("T:" + tupleType.FullName, ","), tupleType.ElementTypes.Select(t => GetLink(item, t)))
                    + GetLink("T:" + tupleType.FullName, "&gt;");
            }

            return element switch
            {
                IType type => type.Kind switch
                {
                    TypeKind.Array when type is TypeWithElementType arrayType => GetLink(item, arrayType.ElementType) + GetLink("T:System.Array", "[]"),
                    TypeKind.FunctionPointer when type is FunctionPointerType functionPointerType => HandleFunctionPointer(functionPointerType),
                    TypeKind.Pointer when type is TypeWithElementType pointerType => GetLink(item, pointerType.ElementType) + "*",
                    TypeKind.ByReference when type is TypeWithElementType innerType => GetLink(item, innerType.ElementType),
                    TypeKind.TypeParameter => item.TryGetTypeParameterDocItem(type.Name, out TypeParameterDocItem typeParameter) ? GetLink(typeParameter) : type.Name,
                    TypeKind.Dynamic => ToLink("https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/types/using-type-dynamic", "dynamic"),
                    TypeKind.Tuple when type is TupleType tupleType => HandleTupleType(tupleType),
                    TypeKind.Unknown => GetLink("?:" + type.FullName),
                    _ when type is ParameterizedType genericType => HandleParameterizedType(genericType),
                    _ => GetLink(type.GetDefinition().GetIdString())
                },
                IMember member => GetLink(member.MemberDefinition.GetIdString(), DocItem.NameAmbience.ConvertSymbol(member)),
                IEntity entity => GetLink(entity.GetIdString(), DocItem.NameAmbience.ConvertSymbol(entity)),
                _ => element.FullName
            };
        }

        private void WriteText(DocItem item, XElement element, string title = null, bool isPreview = false)
        {
            string text = GetText(item, element, title, isPreview);

            if (text is null)
            {
                return;
            }

            if (isPreview)
                _builder.Append(text);
            else
                _builder.AppendLine(text);
        }

        private string GetText(DocItem item, XElement element, string title = null, bool isPreview = false)
        {
            if (element is null)
            {
                return null;
            }

            StringBuilder returnBuilder = new StringBuilder();

            if (title is not null)
            {
                returnBuilder.AppendLine(title);
            }

            int textStart = returnBuilder.Length;
            int? startIndex = default;
            bool isNewLine = true;

            StringBuilder WriteText(string text)
            {
                string[] lines = text.Split('\n');
                int currentLine = 0;

                if (startIndex is null && isNewLine)
                {
                    for (currentLine = 0; currentLine <= lines.Length; ++currentLine)
                    {
                        string line = lines[currentLine];
                        if (!string.IsNullOrWhiteSpace(line))
                        {
                            startIndex = line.Length - line.TrimStart().Length;
                            break;
                        }
                    }
                }

                for (; currentLine < lines.Length; ++currentLine)
                {
                    string line = lines[currentLine];
                    if (isNewLine)
                    {
                        returnBuilder.Append(line, Math.Min(line.Length, startIndex ?? 0), Math.Max(0, line.Length - (startIndex ?? 0)));
                    }
                    else
                    {
                        returnBuilder.Append(line);
                    }

                    isNewLine = currentLine < lines.Length - 1;
                    if (isNewLine)
                    {
                        if (isPreview)
                        {
                            if (_ignoreLineBreak)
                            {
                                returnBuilder.Append(' ');
                            }
                            else
                            {
                                returnBuilder.Append("<br/>");
                            }
                        }
                        else if (_ignoreLineBreak)
                        {
                            returnBuilder.AppendLine();
                        }
                        else
                        {
                            returnBuilder.AppendLine("  ");
                        }
                    }
                }

                return returnBuilder;
            }

            StringBuilder WriteSee(XElement element)
            {
                string @ref = element.GetCRefAttribute();
                if (@ref is not null)
                {
                    return returnBuilder.Append(GetLink(@ref, element.Value.NullIfEmpty()));
                }

                @ref = element.GetHRefAttribute();
                if (@ref is not null)
                {
                    return returnBuilder.Append(ToLink(@ref, element.Value.NullIfEmpty()));
                }

                @ref = element.GetLangWordAttribute();
                if (@ref is not null)
                {
                    return returnBuilder.Append(ToLink(
                        @ref switch
                        {
                            "await" => "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/await",
                            "false" => "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/bool",
                            "true" => "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/bool",
                            _ => $"https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/{@ref}"
                        },
                        element.Value.NullIfEmpty() ?? @ref));
                }

                return returnBuilder;
            }

            StringBuilder WritePara(XElement element)
            {
                Func<StringBuilder> lineBreak = isPreview ? () => returnBuilder.Append("<br/><br/>") : () => returnBuilder.AppendLine().AppendLine();

                if (textStart < returnBuilder.Length)
                {
                    lineBreak();
                }

                WriteNodes(element);

                return lineBreak();
            }

            StringBuilder WriteCode(XElement element)
            {
                using RollbackSetter<bool> _ = new(() => ref _ignoreLineBreak, true);

                returnBuilder.Append("```").AppendLine(element.GetLanguageAttribute() ?? "csharp");

                string source = element.GetSourceAttribute();

                if (source is null)
                {
                    WriteText(element.Value);
                }
                else
                {
                    int? previousStartIndex = startIndex;
                    isNewLine = true;
                    startIndex = null;

                    WriteText(GetCode(source, element.GetRegionAttribute()));

                    startIndex = previousStartIndex;
                }

                return returnBuilder.AppendLine("```");
            }

            void WriteNodes(XElement parent)
            {
                using (new RollbackSetter<bool>(() => ref _ignoreLineBreak, parent.GetIgnoreLineBreak() ?? _ignoreLineBreak))
                {
                    foreach (XNode node in parent.Nodes())
                    {
                        _ = node switch
                        {
                            XText text => WriteText(text.Value),
                            XElement element => element.Name.ToString() switch
                            {
                                "see" => WriteSee(element),
                                "seealso" => returnBuilder,
                                "typeparamref" => returnBuilder.Append(item.TryGetTypeParameterDocItem(element.GetNameAttribute(), out TypeParameterDocItem typeParameter) ? GetLink(typeParameter) : element.GetNameAttribute()),
                                "paramref" => returnBuilder.Append(item.TryGetParameterDocItem(element.GetNameAttribute(), out ParameterDocItem parameter) ? GetLink(parameter) : element.GetNameAttribute()),
                                "c" => returnBuilder.Append('`').Append(element.Value).Append('`'),
                                "code" => isPreview ? returnBuilder : WriteCode(element),
                                "para" => WritePara(element),
                                _ => WriteText(element.ToString()),
                            },
                            _ => throw new Exception($"unhandled node type in summary {node.NodeType}")
                        };

                        if (node is XElement)
                        {
                            isNewLine = false;
                        }
                    }
                }
            }

            WriteNodes(element);

            if (!isPreview)
            {
                returnBuilder.AppendLine();

                int builderLength = -1;
                while (returnBuilder.Length != builderLength && returnBuilder.Length > Environment.NewLine.Length * 2)
                {
                    builderLength = returnBuilder.Length;
                    returnBuilder.Replace(Environment.NewLine + Environment.NewLine, Environment.NewLine, builderLength - (Environment.NewLine.Length * 2), Environment.NewLine.Length * 2);
                }
            }

            string returnString = returnBuilder.ToString().TrimEnd('\r', '\n', ' ');

            if (returnString.EndsWith("<br/>"))
                return returnString.Substring(0, returnString.Length-5);
            else
                return returnString;
        }

        private void WriteNamespace(DocItem item)
        {
            AssemblyDocItem assembly = Items.OfType<AssemblyDocItem>().Single();

            Stack<DocItem> parents = new();
            for (DocItem parent = item?.Parent; parent != assembly && parent != null; parent = parent.Parent)
            {
                parents.Push(parent);
            }

            if (parents.Count > 0)
            {
                _builder.Append("- **Namespace:** ").AppendLine(string.Join(".", parents.Select(p => GetLink(p))));
            }
        }

        private string GetTitle(DocItem item, string prefix = "# ")
        {
            if (String.IsNullOrWhiteSpace(prefix))
                prefix = String.Empty;

            return item switch
            {
                NamespaceDocItem => $"{prefix}{item.Name} Namespace",
                TypeDocItem typeItem => $"{prefix}{item.LongName} {typeItem.Type.Kind}",
                ConstructorDocItem => $"{prefix}{item.LongName} Constructor",
                EventDocItem => $"{prefix}{item.LongName} Event",
                FieldDocItem => $"{prefix}{item.LongName} Field",
                MethodDocItem => $"{prefix}{item.LongName} Method",
                OperatorDocItem => $"{prefix}{item.LongName} Operator",
                PropertyDocItem => $"{prefix}{item.LongName} Property",
                ExplicitInterfaceImplementationDocItem explicitItem when explicitItem.Member is IMethod => $"{prefix}{item.LongName} Method",
                ExplicitInterfaceImplementationDocItem explicitItem when explicitItem.Member is IProperty => $"{prefix}{item.LongName} Property",
                EnumFieldDocItem enumFiedItem => $"`{item.Name}` {enumFiedItem.Field.GetConstantValue()}  ",
                ParameterDocItem parameterItem => $"`{item.Name}` {GetLink(item, parameterItem.Parameter.Type)}  ",
                TypeParameterDocItem typeParameterItem => $"`{typeParameterItem.TypeParameter.Name}`  ",
                _ => null
            };
        }

        private void WriteTitle(DocItem item)
        {
            if (!HasOwnPage(item))
            {
                string url = GetUrl(item);
                int startIndex = url.IndexOf('#') + 1;
                _builder.Append("<a name='").Append(url, startIndex, url.Length - startIndex).AppendLine("'></a>");
            }

            string title = GetTitle(item);

            if (title is not null)
            {
                _builder.AppendLine(title);

                if (item is not ParameterDocItem)
                {
                    _builder.AppendLine();
                }
            }
        }

        private void WriteAssembly(DocItem item)
        {
            string assembly = Path.GetFileName(item.Entity.Compilation.MainModule.PEFile.FileName);
            
            _builder.AppendLine($"- **Assembly:** {assembly}");
        }

        private void WriteDefinition(DocItem item)
        {
            if (item is IDefinedDocItem definedItem)
            {
                _builder.AppendLine("## Definition");

                WriteNamespace(item);
                WriteAssembly(item);

                _builder.AppendLine();

                WriteText(item, GetSummary(item));

                _builder.AppendLine();
                _builder.AppendLine("```csharp");
                definedItem.WriteDefinition(_builder);
                _builder.AppendLine("```");
            }
        }

        private void WriteReturns(DocItem item)
        {
            IType returnType = item switch
            {
                DelegateDocItem delegateItem => delegateItem.InvokeMethod.ReturnType,
                MethodDocItem methodItem => methodItem.Method.ReturnType,
                OperatorDocItem operatorItem => operatorItem.Method.ReturnType,
                _ => null
            };

            if (returnType != null && returnType.Kind != TypeKind.Void)
            {
                _builder.AppendLine("#### Returns");
                _builder.Append(GetLink(item, returnType)).AppendLine("  ");
                WriteText(item, item.Documentation.GetReturns());
            }
        }

        private void WriteEventType(DocItem item)
        {
            if (item is EventDocItem eventItem)
            {
                _builder.AppendLine("#### Event Type");
                _builder.AppendLine(GetLink(item, eventItem.Event.ReturnType));
            }
        }

        private void WriteFieldValue(DocItem item)
        {
            if (item is FieldDocItem fieldItem)
            {
                _builder.AppendLine("#### Field Value");
                _builder.AppendLine(GetLink(item, fieldItem.Field.Type));
            }
        }

        private void WritePropertyValue(DocItem item)
        {
            if (item is PropertyDocItem propertyItem)
            {
                _builder.AppendLine("#### Property Value");
                _builder.AppendLine(GetLink(item, propertyItem.Property.ReturnType));
                WriteText(item, item.Documentation.GetValue());
            }
        }

        private void WriteExceptions(DocItem item)
        {
            bool hasTitle = false;
            foreach (XElement exception in item.Documentation.GetExceptions())
            {
                if (!hasTitle)
                {
                    hasTitle = true;
                    _builder.AppendLine("#### Exceptions");
                }

                string cref = exception.GetCRefAttribute();

                _builder.Append(GetLink(cref)).AppendLine("  ");

                WriteText(item, exception);
            }
        }

        private void WriteInheritances(DocItem item)
        {
            if (item is TypeDocItem typeItem)
            {
                if (typeItem.Type.Kind == TypeKind.Class)
                {
                    _builder.AppendLine().Append("Inheritance ");
                    foreach (IType t in typeItem.Type.GetNonInterfaceBaseTypes().Where(t => t != typeItem.Type))
                    {
                        _builder.Append(GetLink(item, t)).Append(" &rarr; ");
                    }
                    _builder.Append(item.Name).AppendLine("  ");
                }

                List<TypeDocItem> derived = Items.OfType<TypeDocItem>().Where(i => i.Type.DirectBaseTypes.Select(t => t.GetDefinition() ?? t).Contains(typeItem.Type)).OrderBy(i => i.FullName).ToList();
                if (derived.Count > 0)
                {
                    _builder.AppendLine().Append("Derived");
                    foreach (TypeDocItem t in derived)
                    {
                        _builder.Append("  ").Append(Environment.NewLine).Append("&#8627; ").Append(GetLink(t));
                    }
                    _builder.AppendLine("  ");
                }
            }
        }

        private void WriteImplements(DocItem item)
        {
            IEnumerable<INamedElement> GetImplementation(IMember member)
            {
                string id = member.GetIdString().Substring(member.DeclaringTypeDefinition.GetIdString().Length);

                return member
                    .DeclaringTypeDefinition
                    .GetBaseTypeDefinitions()
                    .Where(t => t.Kind == TypeKind.Interface && t.GetDefinition().Accessibility == Accessibility.Public)
                    .SelectMany(t => t.Members)
                    .Where(e => e.GetIdString().Substring(e.DeclaringTypeDefinition.GetIdString().Length) == id);
            }

            List<INamedElement> implementations = (item switch
            {
                TypeDocItem typeItem => typeItem.Type.DirectBaseTypes.Where(t => t.Kind == TypeKind.Interface && t.GetDefinition().Accessibility == Accessibility.Public).OfType<INamedElement>(),
                PropertyDocItem propertyItem => GetImplementation(propertyItem.Property),
                MethodDocItem methodItem => GetImplementation(methodItem.Method),
                ExplicitInterfaceImplementationDocItem explicitItem => explicitItem.Member.ExplicitlyImplementedInterfaceMembers,
                _ => Enumerable.Empty<INamedElement>()
            }).ToList();

            if (implementations.Count > 0)
            {
                _builder.AppendLine().Append("Implements ");
                foreach (INamedElement i in implementations)
                {
                    _builder.Append(GetLink(item, i)).Append(", ");
                }
                _builder.Length -= 2;
                _builder.AppendLine("  ");
            }
        }

        private void WriteSeeAlsos(DocItem item)
        {
            bool hasTitle = false;
            foreach (XElement seeAlso in item.Documentation.GetSeeAlsos())
            {
                if (!hasTitle)
                {
                    hasTitle = true;
                    _builder.AppendLine("#### See Also");
                }

                string @ref = seeAlso.GetCRefAttribute();
                if (@ref is not null)
                {
                    _builder.Append("- ").AppendLine(GetLink(@ref, seeAlso.Value.NullIfEmpty()));
                    continue;
                }

                @ref = seeAlso.GetHRefAttribute();
                if (@ref is not null)
                {
                    _builder.Append("- ").AppendLine(ToLink(@ref, seeAlso.Value.NullIfEmpty()));
                }
            }
        }

        private void WriteItems(IEnumerable<DocItem> items, string title = null, string tableTitle = null)
        {
            if (items is null || !items.Any())
            {
                return;
            }

            _builder.AppendLine();
            _builder.AppendLine(title);

            static IEnumerable<DocItem> GetAllDeclaringTypes(DocItem item)
            {
                while (item is TypeDocItem)
                {
                    yield return item;
                    item = item.Parent;
                }
            }

            foreach (DocItem item in items ?? Enumerable.Empty<DocItem>())
            {
                if (title is not null)
                {
                    if (HasOwnPage(item))
                    {
                        _builder.AppendLine().Append("| ").Append(tableTitle ?? String.Empty).AppendLine(" | |").AppendLine("| :--- | :--- |");
                    }
                    // else
                    // {
                    //     _builder.AppendLine(title);
                    // }
                    title = null;
                }

                if (HasOwnPage(item))
                {
                    _builder
                        .Append("| ")
                        .Append(GetLink(item, item is TypeDocItem ? string.Join(".", GetAllDeclaringTypes(item).Reverse().Select(i => i.Name)) : null))
                        .Append(" | ");

                    WriteText(item, item.Documentation.GetSummary(), null, true);

                    _builder.AppendLine(" |");
                }
                else
                {
                    WriteItem(item);
                    _builder.AppendLine("  ");
                }
            }
        }

        private void WriteFrontMatter(DocItem item)
        {
            string title = GetTitle(item, null);
            string description = GetText(item, GetSummary(item))?.Trim();

            _builder.AppendLine("---");
            _builder.AppendLine($"title: {title}");
            
            if (!String.IsNullOrWhiteSpace(description))
            {
                string[] descriptionLines = description.Split('\n');

                if (descriptionLines.Length > 0)
                {
                    _builder.AppendLine($"description: >-");

                    foreach (string line in descriptionLines)
                    {
                        _builder.AppendLine($"  {line}");
                    }
                }
            }
            _builder.AppendLine("---");
        }

        private XElement GetSummary(DocItem item) => item switch
        {
            TypeParameterDocItem => item.Documentation,
            ParameterDocItem => item.Documentation,
            _ => item.Documentation.GetSummary()
        };

        private void WriteParameterDescription(DocItem item)
        {
            if (item is not ParameterDocItem)
            {
                return;
            }

            WriteText(item, GetSummary(item));
        }

        private void WriteItem(DocItem item)
        {
            WriteTitle(item);
            WriteParameterDescription(item);
            WriteDefinition(item);

            WriteItems((item as ITypeParameterizedDocItem)?.TypeParameters, "## Type parameters", "Type Parameter");
            WriteItems((item as IParameterizedDocItem)?.Parameters, "## Parameters", "Parameter");
            WriteItems(GetChildren<EnumFieldDocItem>(item), "## Fields", "Field");
            WriteEventType(item);
            WriteFieldValue(item);
            WritePropertyValue(item);
            WriteReturns(item);
            WriteExceptions(item);
            WriteInheritances(item);
            // todo: attribute
            WriteImplements(item);

            if (item.Documentation.GetExample() is not null)
            {
                _builder.AppendLine();
                WriteText(item, item.Documentation.GetExample(), "## Example");
            }

            if (item.Documentation.GetRemarks() is not null)
            {
                _builder.AppendLine();
                WriteText(item, item.Documentation.GetRemarks(), "## Remarks");
            }

            WriteItems(GetChildren<ConstructorDocItem>(item), "## Constructors", "Constructor");
            WriteItems(GetChildren<FieldDocItem>(item), "## Fields", "Field");
            WriteItems(GetChildren<PropertyDocItem>(item), "## Properties", "Property");
            WriteItems(GetChildren<MethodDocItem>(item), "## Methods", "Method");
            WriteItems(GetChildren<EventDocItem>(item), "## Events", "Event");
            WriteItems(GetChildren<OperatorDocItem>(item), "## Operators", "Operator");
            WriteItems(GetChildren<ExplicitInterfaceImplementationDocItem>(item), "## Explicit Interface Implementations", "Implementation");

            WriteItems(GetChildren<ClassDocItem>(item), "## Classes", "Class");
            WriteItems(GetChildren<StructDocItem>(item), "## Structs", "Struct");
            WriteItems(GetChildren<InterfaceDocItem>(item), "## Interfaces", "Interface");
            WriteItems(GetChildren<EnumDocItem>(item), "## Enums", "Enum");
            WriteItems(GetChildren<DelegateDocItem>(item), "## Delegates", "Delegate");

            WriteItems(GetChildren<NamespaceDocItem>(item), "## Namespaces", "Namespace");

            WriteSeeAlsos(item);
        }

        protected override void Clean(DirectoryInfo directory)
        {
            if (directory.Exists)
            {
                IEnumerable<FileInfo> files = directory.EnumerateFiles("*.md").Where(f => !string.Equals(f.Name, "readme.md", StringComparison.OrdinalIgnoreCase));

                int i;

                foreach (FileInfo file in files)
                {
                    i = 3;
                start:
                    try
                    {
                        file.Delete();
                    }
                    catch
                    {
                        if (--i > 0)
                        {
                            Thread.Sleep(100);
                            goto start;
                        }

                        throw;
                    }
                }

                i = 3;
                while (files.Any() && i-- > 0)
                {
                    Thread.Sleep(1000);
                }
            }
            else
            {
                directory.Create();
            }
        }

        protected override string GetUrl(DocItem item) => _urls.GetOrAdd(item.Id, _ => _urlFactory(item));

        protected override void WritePage(DirectoryInfo directory, DocItem item)
        {
            _builder.Clear();

            WriteFrontMatter(item);

            WriteItem(item);

            _builder.Replace(" />", "/>");

            File.WriteAllText(Path.Combine(directory.FullName, GetFileName(item) + ".md"), _builder.ToString());
        }
    }
}
