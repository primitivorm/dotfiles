SELECT  sc.name + '.' + t.NAME AS TableName,  
        p.[Rows],  
        ( SUM(a.total_pages) * 8 ) / 1024 AS TotalReservedSpaceMB, -- Number of total pages * 8KB size of each page in SQL Server  
        ( SUM(a.used_pages) * 8 ) / 1024 AS UsedDataSpaceMB,  
        ( SUM(a.data_pages) * 8 ) / 1024 AS FreeUnusedSpaceMB  
FROM    .sys.tables t  
        INNER JOIN sys.schemas sc ON sc.schema_id = t.schema_id  
        INNER JOIN sys.indexes i ON t.OBJECT_ID = i.object_id  
        INNER JOIN sys.partitions p ON i.object_id = p.OBJECT_ID AND i.index_id = p.index_id  
        INNER JOIN .sys.allocation_units a ON p.partition_id = a.container_id  
WHERE   t.type_desc = 'USER_TABLE'  
        AND i.index_id <= 1  --- Heap\ CLUSTERED
        --AND t.NAME='BitacoraError' -- Replace with valid table name
GROUP BY sc.name + '.' + t.NAME,  
        i.[object_id],i.index_id, i.name, p.[Rows]  
ORDER BY TotalReservedSpaceMB desc 


