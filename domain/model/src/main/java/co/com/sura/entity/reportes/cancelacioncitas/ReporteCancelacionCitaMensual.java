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
public class ReporteCancelacionCitaMensual extends ReporteCancelacionCita{
    @JsonIgnore
    private List<RegistroCancelacionCitaMensual> reportes;

    public static Mono<ReporteCancelacionCitaMensual> calcularResumen(ReporteCancelacionCitaMensual reporte){
        return Flux.fromIterable(reporte.getReportes())
                .groupBy(RegistroCancelacionCitaMensual::getDescripcion)
                .flatMap(groupFlux -> groupFlux
                        .reduce((r1, r2) -> RegistroCancelacionCitaMensual.builder()

                                .descripcion("")
                                .cantidad(r1.getCantidad() + r2.getCantidad())
                                .build())
                        .map(RegistroCancelacionCitaMensual::toBuilder)
                        .map(builder -> builder.descripcion(groupFlux.key()))
                        .map(RegistroCancelacionCitaMensual.RegistroCancelacionCitaMensualBuilder::build))

                .map(reportesResumen -> {
                    var resumen = Optional.ofNullable(reporte.getResumen())
                            .orElse(ResumenCancelacionCita.builder().registrosCancelacion(new ArrayList<>()).build());

                    reportesResumen.setDia(null);
                    resumen.getRegistrosCancelacion().add(reportesResumen);
                    resumen.setRegistrosCancelacion(resumen.getRegistrosCancelacion());
                    resumen.setTotalCancelaciones(resumen.getTotalCancelaciones()+reportesResumen.getCantidad());
                    reporte.setResumen(resumen);
                    return reporte;
                })
                .collectList().thenReturn(reporte);
    }
}
