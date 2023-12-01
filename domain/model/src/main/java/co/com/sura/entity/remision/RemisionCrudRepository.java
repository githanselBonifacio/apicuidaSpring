package co.com.sura.entity.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.remision.datosremision.DatosAtencionPaciente;
import co.com.sura.entity.remision.datosremision.Paciente;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.List;

public interface RemisionCrudRepository {
    //remisiones
    Flux<Remision>                   consultarRemisiones();
    Mono<Boolean>                    crearRemisionCita (RemisionRequest remisionRequest, List<CitaRequest> citas);

    Mono<Boolean>                    actualizarRemisionPorNovedad(
                                                    RemisionRequest remisionRequest,
                                                    List<CitaRequest> citas, NovedadRequest novedadRequest);

    Mono<Boolean>                    egresarRemisionById(String idRemision);

    //datos paciente
    Mono<DatosAtencionPaciente>      consultarDatosAtencionPacienteByIdRemision (String idRemision);
    Mono<Paciente>                   consultarPacienteFromRemision (String idRemision);



}
