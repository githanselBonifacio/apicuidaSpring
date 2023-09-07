package co.com.sura.web.remision;


import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.dto.remision.CrearRemisionCitasRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.remision.DatosAtencionPaciente;
import co.com.sura.entity.remision.Paciente;
import co.com.sura.entity.remision.RegistroHistorialRemision;
import co.com.sura.entity.remision.Remision;
import co.com.sura.genericos.Response;
import co.com.sura.remision.RemisionUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.List;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/remision")
public class RemisionController {

    @Autowired
    private RemisionUseCase remisionUseCase;

    //remision
    @PostMapping(value = "/crearRemisionCitas")
    public Mono<Response<Boolean>> crearRemisioCitas(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest) {
        return remisionUseCase.crearRemisionCitas(
                        crearRemisionCitasRequest.getRemision(),
                        crearRemisionCitasRequest.getCitas())
                .map(fueCreada -> ResponseFactory.createStatus(
                        fueCreada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.REMISION_CREADA.getValue(),
                        Mensajes.REMISION_CREADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        e.getMessage().contains(
                                Mensajes.REMISION_EXISTENTE.getValue().split("d")[0])?
                                StatusCode.STATUS_400.getValue():StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_REMISION.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()

                )));
    }

    @GetMapping(value = "")
    public Flux<Remision> consultarRemisiones(){
        return remisionUseCase.consultarRemisiones();
    }

    @GetMapping(value = "datosAtencionPaciente/{idRemision}")
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByRemision(@PathVariable String idRemision){
        return remisionUseCase.consultarDatosAtencionPacienteByRemision(idRemision);
    }

    @GetMapping(value = "pacienteFromRemision/{idRemision}")
    public Mono<Paciente> consultarPacienteFromRemision(@PathVariable String idRemision){
        return remisionUseCase.consultarPacienteFromRemision(idRemision);
    }

    @GetMapping(value = "tratamientosFarmacia")
    public Flux<PacienteTratamientoCita> consultarMedicamentosToFarmacia(){
        return remisionUseCase.consultarAllTratamientosToFarmacia();
    }

    @PostMapping(value = "notificarFarmacia")
    public Mono<Void>notificarMedicamentosToFarmacia(@RequestBody List<PacienteTratamientoCita> tratamientoCitasList){
        return remisionUseCase.notificarMedicamentosToFarmacia(tratamientoCitasList);
    }

    @GetMapping(value = "historial/{idRemision}")
    public Flux<RegistroHistorialRemision> consultarHistorialRemisionById(@PathVariable String idRemision){
        return remisionUseCase.consultarHistorialRemisionById(idRemision);
    }

    @GetMapping(value = "/{idRemision}")
    public Mono<RegistroHistorialRemision> consultarAllDataRemisionById(@PathVariable String idRemision){
        return remisionUseCase.consultarDataActualRemision(idRemision);
    }

    @PostMapping("/actualizarRemisionPorNovedad")
    public Mono<Void> actualizarRemisionPorNovedad(@RequestBody CrearRemisionCitasRequest crearRemisionCitasRequest){
        return remisionUseCase.actualizarRemisionPorNovedad(
                crearRemisionCitasRequest.getRemision(),
                crearRemisionCitasRequest.getCitas(),
                crearRemisionCitasRequest.getNovedad()
        );
    }

    @PostMapping(value="/egresar/{idRemision}")
    public Mono<Response<Boolean>> egresarRemisionById(@PathVariable String idRemision) {
        return remisionUseCase.egresarRemisionById(idRemision)
                .map(fueEgresada -> ResponseFactory.createStatus(
                        fueEgresada,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.REMISION_EGRESADA.getValue(),
                        Mensajes.REMISION_EGRESADA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_EGRESAR_REMISION.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
}
