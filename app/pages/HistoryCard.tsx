"use client";

import React from "react";
import { Clock, Copy, Check } from "lucide-react";

interface HistoryCard {
  id: string;
  legacy_language: string;
  legacy_code: string;
  modern_language: string;
  converted_code: string;
  documentation: string;
  createdAT: Date | string;
  updatedAt: Date | string;
}

const CodeHistoryCard = ({ history }: { history: HistoryCard }) => {
  const [copiedField, setCopiedField] = React.useState<string | null>(null);
  const createdAtDate = new Date(history.createdAT);
  const updatedAtDate = new Date(history.updatedAt);

  const copyToClipboard = (text: string, field: string) => {
    navigator.clipboard.writeText(text);
    setCopiedField(field);
    setTimeout(() => setCopiedField(null), 2000);
  };

  const CodeBlock = ({
    title,
    code,
    field,
  }: {
    title: string;
    code: string;
    field: string;
  }) => (
    <div className="space-y-2">
      <div className="flex justify-between items-center">
        <h3 className="text-lg font-semibold text-gray-800">{title}</h3>
        <button
          onClick={() => copyToClipboard(code, field)}
          className="inline-flex items-center px-3 py-1.5 text-sm bg-gray-100 hover:bg-gray-200 text-gray-700 rounded-lg font-medium transition-colors duration-200"
        >
          {copiedField === field ? (
            <>
              <Check className="h-4 w-4 mr-1.5" />
              Copied!
            </>
          ) : (
            <>
              <Copy className="h-4 w-4 mr-1.5" />
              Copy
            </>
          )}
        </button>
      </div>
      <pre className="p-4 bg-gray-50 border border-gray-200 rounded-lg overflow-x-auto font-mono text-sm">
        {code}
      </pre>
    </div>
  );

  return (
    <div className="bg-white rounded-2xl shadow-xl border border-gray-100 p-8 max-w-4xl w-full mx-auto">
      <div className="flex items-center gap-2 text-sm text-gray-500 mb-6">
        <Clock className="h-4 w-4" />
        Created{" "}
        {createdAtDate.toLocaleDateString("en-US", {
          year: "numeric",
          month: "long",
          day: "numeric",
          hour: "2-digit",
          minute: "2-digit",
        })}
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        <div className="p-4 bg-gray-50 rounded-lg border border-gray-200">
          <p className="text-sm font-medium text-gray-500 mb-1">
            Legacy Language
          </p>
          <p className="text-lg font-semibold text-gray-900">
            {history.legacy_language}
          </p>
        </div>
        <div className="p-4 bg-gray-50 rounded-lg border border-gray-200">
          <p className="text-sm font-medium text-gray-500 mb-1">
            Modern Language
          </p>
          <p className="text-lg font-semibold text-gray-900">
            {history.modern_language}
          </p>
        </div>
      </div>

      <div className="space-y-8">
        <CodeBlock
          title="Legacy Code"
          code={history.legacy_code}
          field="legacy"
        />
        <CodeBlock
          title="Converted Code"
          code={history.converted_code}
          field="converted"
        />

        <div className="space-y-2">
          <h3 className="text-lg font-semibold text-gray-800">Documentation</h3>
          <div className="p-4 bg-gray-50 border border-gray-200 rounded-lg prose prose-gray max-w-none">
            {history.documentation}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CodeHistoryCard;
