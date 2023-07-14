package co.com.sura.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.remision.DatosAtencionPaciente;
import co.com.sura.entity.remision.Paciente;
import co.com.sura.entity.remision.RemisionFactory;
import co.com.sura.entity.remision.RemisionCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

public class RemisionUseCase implements RemisionFactory {

   private final RemisionCrudRepository remisionCrudRepository;

    public RemisionUseCase(RemisionCrudRepository remisionRepository) {
        this.remisionCrudRepository = remisionRepository;
    }

    public Mono<Void> crearRemisionCitas (RemisionRequest remisionRequest, List<CitaRequest> citas){
        return remisionCrudRepository
                .crearRemisionCita(
                        remisionRequest,
                        citas
                );
    }
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByRemision(String idRemision){
        return remisionCrudRepository.consultarDatosAtencionPacienteByIdRemision(idRemision);
    }

    public Mono<Paciente> consultarPacienteFromRemision(String idRemision){
        return remisionCrudRepository.consultarPacienteFromRemision(idRemision);
    }
}
