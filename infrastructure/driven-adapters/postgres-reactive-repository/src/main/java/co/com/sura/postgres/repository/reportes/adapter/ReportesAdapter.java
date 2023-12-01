package co.com.sura.postgres.repository.reportes.adapter;

import co.com.sura.entity.maestro.Regional;
import co.com.sura.entity.reportes.ReportesRepository;
import co.com.sura.entity.reportes.cancelacioncitas.RegistroCancelacionCitaAnual;
import co.com.sura.entity.reportes.cancelacioncitas.RegistroCancelacionCitaMensual;
import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.entity.reportes.cancelacioncitas.ResumenCancelacionCita;
import co.com.sura.entity.reportes.turnos.ItemReporteAnual;
import co.com.sura.entity.reportes.turnos.ItemReporteMensual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoAnual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoMensual;
import co.com.sura.postgres.repository.maestros.adapter.ConverterMaestros;
import co.com.sura.postgres.repository.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.repository.reportes.querys.QueryReportes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.r2dbc.core.R2dbcEntityTemplate;
import org.springframework.data.r2dbc.repository.config.EnableR2dbcRepositories;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.reactive.TransactionalOperator;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.List;


@Repository
@EnableR2dbcRepositories
public class ReportesAdapter implements ReportesRepository {
    private final RegionalesRepository regionalesRepository;
    private final R2dbcEntityTemplate r2dbcEntityTemplate;
    private final TransactionalOperator transactionalOperator;

    @Autowired
    public ReportesAdapter(RegionalesRepository regionalesRepository, R2dbcEntityTemplate r2dbcEntityTemplate,
                           TransactionalOperator transactionalOperator) {

        this.regionalesRepository = regionalesRepository;
        this.r2dbcEntityTemplate = r2dbcEntityTemplate;
        this.transactionalOperator = transactionalOperator;
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
                .flatMap(ReporteTurnoAnual::setNombreMes)
                .flatMap(ReporteTurnoAnual::calcularResumen);
    }

    @Override
    public Mono<ReporteTurnoMensual> consultarReporteMensual(Integer anio, Integer numeroMes, String idRegional) {
        Mono<Regional> regionalResult = regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToCiudad);

        Mono<List<ItemReporteMensual>> itemsReporteMensual = consultarItemsReporteMensual(anio,numeroMes, idRegional)
                .collectList();
        return Mono.zip(regionalResult, itemsReporteMensual)
                .map(tuple -> ReporteTurnoMensual
                        .builder()
                        .regional(tuple.getT1())
                        .reportes(tuple.getT2())
                        .build())
                .flatMap(ReporteTurnoMensual::calcularResumen);
    }

    @Override
    public Mono<ReporteCancelacionCitaAnual> consultaReporteCancelacionCitasAnual(Integer anio, String idRegional) {
        Mono<Regional> regionalResult = regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToCiudad);

      Mono<List<RegistroCancelacionCitaAnual>> registroCancelacion =
                               consultarRegistroCancelacionCitaAnual(anio, idRegional)
                                       .collectList();

        return Mono.zip(regionalResult,registroCancelacion)
                .map(tuple->ReporteCancelacionCitaAnual
                        .builder()
                        .regional(tuple.getT1())
                        .reportes(tuple.getT2())
                        .resumen(new ResumenCancelacionCita())
                        .build())
                .flatMap(ReporteCancelacionCitaAnual::agruparReportes)
                .flatMap(ReporteCancelacionCitaAnual::calcularResumen);
    }

    @Override
    public Mono<ReporteCancelacionCitaMensual> consultaReporteCancelacionCitasMensual(
            Integer anio, Integer numeroMes, String idRegional) {
           Mono<Regional> regionalResult = regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToCiudad);

        Mono<List<RegistroCancelacionCitaMensual>> registroCancelacion =
                consultarRegistroCancelacionCitaMensual(anio,numeroMes, idRegional)
                        .collectList();

        return Mono.zip(regionalResult,registroCancelacion)
                .map(tuple->ReporteCancelacionCitaMensual
                        .builder()
                        .regional(tuple.getT1())
                        .reportes(tuple.getT2())
                        .build())
                .flatMap(ReporteCancelacionCitaMensual::agruparReportes)
                .flatMap(ReporteCancelacionCitaMensual::calcularResumen);
    }

    //querys
    //reporte turno
    private Flux<ItemReporteAnual> consultarItemsReporteAnual(Integer anio, String idRegional){
        return transactionalOperator
                .execute(status -> r2dbcEntityTemplate.getDatabaseClient()
                        .sql(QueryReportes.FIND_REPORTE_ANUAL_BY_YEAR_REGIONAL.getQuery())
                        .bind("$1",anio)
                        .bind("$2",idRegional)
                        .map(ConvertReporte::buildItemReporteAnualFromRow)
                        .all());
    }

    private Flux<ItemReporteMensual> consultarItemsReporteMensual(Integer anio,Integer mes, String idRegional){
        return transactionalOperator
                .execute(status -> r2dbcEntityTemplate.getDatabaseClient()
                        .sql(QueryReportes.FIND_REPORTE_MENSUAL_BY_YEAR_REGIONAL.getQuery())
                        .bind("$1",mes)
                        .bind("$2",anio)
                        .bind("$3",idRegional)
                        .map(ConvertReporte::buildItemReporteMensualFromRow)
                        .all());
    }

    //reporte cancelacion citas
    private Flux<RegistroCancelacionCitaAnual> consultarRegistroCancelacionCitaAnual(Integer anio, String idRegional){
        return transactionalOperator
                .execute(status  ->r2dbcEntityTemplate.getDatabaseClient()
                .sql(QueryReportes.FIND_MOTIVOS_CANCELACION_ANUAL.getQuery())
                .bind("$1",idRegional)
                .bind("$2",anio)
                .map(ConvertReporte::buildRegistroCancelacionCitaAnualFromRow)
                .all());
    }

    private Flux<RegistroCancelacionCitaMensual> consultarRegistroCancelacionCitaMensual(
            Integer anio,Integer numeroMes, String idRegional){
        return transactionalOperator
                .execute(status  ->r2dbcEntityTemplate.getDatabaseClient()
                        .sql(QueryReportes.FIND_MOTIVOS_CANCELACION_MENSUAL.getQuery())
                        .bind("$1",idRegional)
                        .bind("$2",numeroMes)
                        .bind("$3",anio)
                        .map(ConvertReporte::buildRegistroCancelacionCitaMensualFromRow)
                        .all());
    }
}
