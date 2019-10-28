﻿using System.Linq;
using System.Xml.Linq;
using DefaultDocumentation.Helper;
using DefaultDocumentation.Model.Base;

namespace DefaultDocumentation.Model.NonMember
{
    internal sealed class ExceptionItem : AItem
    {
        public override string Header => "Exceptions";

        public string Reference => Summary.GetReferenceName();

        private ExceptionItem(AMemberItem parent, XElement element)
            : base(parent, element)
        { }

        public static ExceptionItem[] GetFrom(AMemberItem item)
        {
            return item.Element.GetExceptions()?.Select(i => new ExceptionItem(item, i)).ToArray();
        }

        public override void Write(Converter converter, DocWriter writer)
        {
            writer.Write(
                converter.Items.TryGetValue(Reference, out AMemberItem reference)
                ? reference.AsLink()
                : Reference.Substring(2).AsDotNetApiLink());
            writer.WriteLine("  ");
            converter.WriteSummary(writer, this);
        }
    }
}
