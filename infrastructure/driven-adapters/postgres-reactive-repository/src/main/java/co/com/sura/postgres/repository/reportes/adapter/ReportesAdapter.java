package co.com.sura.postgres.repository.reportes.adapter;

import co.com.sura.entity.maestro.Regional;
import co.com.sura.entity.reportes.ItemReporteAnual;
import co.com.sura.entity.reportes.ItemReporteMensual;
import co.com.sura.entity.reportes.ReporteTurnoAnual;
import co.com.sura.entity.reportes.ReporteTurnoMes;
import co.com.sura.entity.reportes.ReportesRepository;
import co.com.sura.postgres.repository.maestros.adapter.ConverterMaestros;
import co.com.sura.postgres.repository.maestros.data.RegionalesRepository;
import co.com.sura.postgres.repository.reportes.QueryReportes;
import co.com.sura.postgres.repository.reportes.data.ReporteTurnoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.ReactiveTransactionManager;
import org.springframework.transaction.reactive.TransactionalOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;


@Repository
public class ReportesAdapter implements ReportesRepository {

    private final RegionalesRepository regionalesRepository;
    private final R2dbcEntityTemplate r2dbcEntityTemplate;
    private final ReactiveTransactionManager transactionManager;

    @Autowired
    public ReportesAdapter(ReporteTurnoRepository reportesTurnoRepository,
                           RegionalesRepository regionalesRepository, DatabaseClient databaseClient,
                           R2dbcEntityTemplate r2dbcEntityTemplate, ReactiveTransactionManager transactionManager) {

        this.regionalesRepository = regionalesRepository;
        this.r2dbcEntityTemplate = r2dbcEntityTemplate;
        this.transactionManager = transactionManager;
    }


    @Override
    public Mono<ReporteTurnoAnual> consultarReporteAnual(Integer anio, String idRegional) {
        Mono<Regional> regionalResult = regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToCiudad);

        Mono<List<ItemReporteAnual>> itemsReporteAnual = consultarItemsReporteAnual(anio, idRegional)
                .collectList();

        return Mono.zip(regionalResult, itemsReporteAnual)
                .map(tuple -> ReporteTurnoAnual
                        .builder()
                        .regional(tuple.getT1())
                        .reportes(tuple.getT2())
                        .build())
                .map(ReporteTurnoAnual::setNombreMes)
                .map(ReporteTurnoAnual::calcularResumen);
    }

    @Override
    public Mono<ReporteTurnoMes> consultarReporteMensual(Integer anio, Integer numeroMes, String idRegional) {
        Mono<Regional> regionalResult = regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToCiudad);

        Mono<List<ItemReporteMensual>> itemsReporteMensual = consultarItemsReporteMensual(anio,numeroMes, idRegional)
                .collectList();
        return Mono.zip(regionalResult, itemsReporteMensual)
                .map(tuple -> ReporteTurnoMes
                        .builder()
                        .regional(tuple.getT1())
                        .reportes(tuple.getT2())
                        .build())
                .map(ReporteTurnoMes::calcularResumen);
    }

    private Flux<ItemReporteAnual> consultarItemsReporteAnual(Integer anio, String idRegional){
        return TransactionalOperator.create(transactionManager)
                .execute(status -> r2dbcEntityTemplate.getDatabaseClient()
                        .sql(QueryReportes.FIND_REPORTE_ANUAL_BY_YEAR_REGIONAL.getQuery())
                        .bind("$1",anio)
                        .bind("$2",idRegional)
                        .map(ConvertReporte::buildItemReporteAnualFromRow)
                        .all());
    }

    private Flux<ItemReporteMensual> consultarItemsReporteMensual(Integer anio,Integer mes, String idRegional){
        return TransactionalOperator.create(transactionManager)
                .execute(status -> r2dbcEntityTemplate.getDatabaseClient()
                        .sql(QueryReportes.FIND_REPORTE_MENSUAL_BY_YEAR_REGIONAL.getQuery())
                        .bind("$1",mes)
                        .bind("$2",anio)
                        .bind("$3",idRegional)
                        .map(ConvertReporte::buildItemReporteMensualFromRow)
                        .all());
    }
}
