package co.com.sura;

import co.com.sura.postgres.remision.data.datospaciente.*;
import co.com.sura.remision.dto.*;
import co.com.sura.remision.entity.datosremision.Diagnostico;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class RemisionTestData {

    public  RemisionRequest remisionRequest;
    public  List<CitaRequest> citasRequest;

    public RemisionData remisionData;
    public DatosAtencionPacienteData datosAtencionPacienteData;

    public List<RemisionDiagnosticoData> remisionDiagnosticosData;

    public UbicacionData ubicacionData;

    public PacienteData pacienteData;
    public RemisionTestData() {
        String idRemision = "11111";
        String idRegional = "427";
        String municipio = "municipio";
        String programa = "programa";
        String nombreResponsable = "responsable";
        String direccionPrueba = "direccion prueba";
        Diagnostico diagnostico = Diagnostico
                .builder()
                .codigo("xxx")
                .nombre("diagnostico")
                .build();
        RemisionDiagnosticoData remisionDiagnosticoData = RemisionDiagnosticoData
                .builder()
                .codigo("xxx")
                .idRemision(idRemision)
                .nombreDiagnostico("diagnostico")
                .build();
        ProgramaRequest programaRequest = ProgramaRequest
                .builder()
                .idPrograma("1")
                .nombre(programa)
                .build();
        TipoIdentificacionRequest tipoIdentificacionRequest = TipoIdentificacionRequest
                .builder()
                .idTipo("1")
                .nombre("cedula")
                .build();
        TipoAfiliacionRequest tipoAfiliacionRequest = TipoAfiliacionRequest
                .builder()
                .idPlan("1")
                .nombre("tipo plan")
                .build();
        RegionalRequest regionalRequest = RegionalRequest.builder()
                .idRegional("427")
                .build();
        MunicipioRequest municipioRequest = MunicipioRequest
                .builder()
                .idMunicipio("001")
                .nombre(municipio)
                .build();
        //ubicacion
        UbicacionRequest UbicacionRequest =  co.com.sura.remision.dto.UbicacionRequest
                .builder()
                .direccion(direccionPrueba)
                .municipio(municipioRequest)
                .build();
        //datos atencion
        DatosAtencionPacienteRequest datosAtencionPacienteRequest = DatosAtencionPacienteRequest
                .builder()
                .nombreResponsable(nombreResponsable)
                .ubicacion(UbicacionRequest)
                .build();
        //diagnostico
        List<Diagnostico> diagnosticos = new ArrayList<>();
        diagnosticos.add(diagnostico);
        this.remisionRequest = RemisionRequest.builder()
                .idRemision(idRemision)
                .nombre("nombre prueba")
                .apellido("apellido muestra")
                .tipoIdentificacion(tipoIdentificacionRequest)
                .datosAtencionPaciente(datosAtencionPacienteRequest)
                .diagnosticos(diagnosticos)
                .tipoAfiliacion(tipoAfiliacionRequest)
                .programa(programaRequest)
                .regional(regionalRequest)
                .build();

        //citas
        this.citasRequest = new ArrayList<>();
        this.citasRequest.add(
                CitaRequest.builder().build());

        this.remisionData = RemisionData
                .builder()
                .idRemision(idRemision)
                .programa(programa)
                .idRegional(idRegional)
                .build();

        this.datosAtencionPacienteData = DatosAtencionPacienteData
                .builder()
                .idRemision(idRemision)
                .nombreResponsable(nombreResponsable)
                .build();

        this.remisionDiagnosticosData = new ArrayList<>();
        this.remisionDiagnosticosData.add(remisionDiagnosticoData);

        this.ubicacionData = UbicacionData.builder()
                .idUbicacion("null_ubicacion")
                .direccion(direccionPrueba)
                .municipio(municipio)
                .idRegional(idRegional)
                .sinNomenclatura(false)
                .longitud(0.0)
                .latitud(0.0)
                .build();

        this.pacienteData = PacienteData.builder()
                .nombres("nombre prueba")
                .apellidos("apellido muestra")
                .tipoIdentificacion("cedula")
                .tipoAfiliacion("tipo plan")
                .idUbicacion("null_ubicacion")
                .build();
    }
}
