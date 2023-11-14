package co.com.sura.entity.reportes.cancelacioncitas;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Getter
@Setter
@AllArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteCancelacionCitaAnual extends ReporteCancelacionCita{
    @JsonIgnore
    private List<RegistroCancelacionCitaAnual> reportes;

    public static Mono<ReporteCancelacionCitaAnual> setNombreMes(ReporteCancelacionCitaAnual reporte){
        return Flux.fromIterable(reporte.getReportes())
                .map(RegistroCancelacionCitaAnual::converNumeroToNombreMes)
                .collectList()
                .thenReturn(reporte);
    }

    public static Mono<ReporteCancelacionCitaAnual> calcularResumen(ReporteCancelacionCitaAnual reporte){
        return Flux.fromIterable(reporte.getReportes())
              .groupBy(RegistroCancelacionCitaAnual::getDescripcion)
              .flatMap(groupFlux -> groupFlux
                 .reduce((r1, r2) -> RegistroCancelacionCitaAnual.builder()

                                .descripcion("")
                                .cantidad(r1.getCantidad() + r2.getCantidad())
                                .build())
                        .map(RegistroCancelacionCitaAnual::toBuilder)
                        .map(builder -> builder.descripcion(groupFlux.key()))
                        .map(RegistroCancelacionCitaAnual.RegistroCancelacionCitaAnualBuilder::build))

              .map(reportesResumen -> {
                  var resumen = Optional.ofNullable(reporte.getResumen())
                          .orElse(ResumenCancelacionCita.builder().registrosCancelacion(new ArrayList<>()).build());

                  reportesResumen.setMes(null);
                  resumen.getRegistrosCancelacion().add(reportesResumen);
                  resumen.setRegistrosCancelacion(resumen.getRegistrosCancelacion());
                  resumen.setTotalCancelaciones(resumen.getTotalCancelaciones()+reportesResumen.getCantidad());
                  reporte.setResumen(resumen);
                  return reporte;
              })
                .collectList().thenReturn(reporte);
    }

}
