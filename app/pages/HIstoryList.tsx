import { prisma } from "@/lib/prisma";
import Link from "next/link";
import { Clock, ArrowRight } from "lucide-react";

export default async function HistoryList() {
  const historyData = await prisma.history.findMany({
    orderBy: {
      createdAT: "desc",
    },
  });

  if (historyData.length === 0) {
    return (
      <div className="text-center p-8 bg-white rounded-2xl shadow-xl border border-gray-100">
        <h3 className="text-xl font-semibold text-gray-900 mb-2">
          No Conversions Yet
        </h3>
        <p className="text-gray-600 mb-4">
          Start converting your legacy code to see your history here.
        </p>
        <Link
          href="/convert"
          className="inline-flex items-center px-4 py-2 bg-gradient-to-r from-purple-600 to-blue-600 text-white rounded-lg font-semibold hover:from-purple-700 hover:to-blue-700 transition-all duration-200"
        >
          Convert Code
          <ArrowRight className="ml-2 h-4 w-4" />
        </Link>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {historyData.map((item) => (
        <Link
          key={item.id}
          href={`/history/${item.id}`}
          className="block transition-all duration-200 hover:transform hover:translate-y-[-2px]"
        >
          <div className="p-6 bg-white rounded-2xl shadow-xl border border-gray-100 hover:shadow-2xl transition-all duration-200">
            <div className="flex justify-between items-start mb-4">
              <div>
                <div className="flex items-center gap-2 text-sm text-gray-500 mb-2">
                  <Clock className="h-4 w-4" />
                  {new Date(item.createdAT).toLocaleDateString("en-US", {
                    year: "numeric",
                    month: "long",
                    day: "numeric",
                    hour: "2-digit",
                    minute: "2-digit",
                  })}
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-1">
                  Code Conversion
                </h3>
                <p className="text-gray-600">
                  {item.legacy_language} → {item.modern_language}
                </p>
              </div>
              <ArrowRight className="h-5 w-5 text-gray-400" />
            </div>
            <div className="pl-4 border-l-4 border-purple-100">
              <p className="text-sm text-gray-600 line-clamp-2 font-mono">
                {item.legacy_code}
              </p>
            </div>
          </div>
        </Link>
      ))}
    </div>
  );
}
